import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._
import java.io.File
import scala.collection.mutable.ListBuffer

class FrontendSpec extends AnyFlatSpec {
    import testUtils.{waccProgramsInDir, assertResultEquals}
    import parsley.{Failure, Success}
    import parsley.io.ParseFromIO

    implicit val eb = new WaccErrorBuilder

    val syntaxValid = waccProgramsInDir(new File("./programs/valid"))
    val syntaxInvalid = waccProgramsInDir(
      new File("./programs/invalid/syntaxErr")
    )
    val semanticInvalid = waccProgramsInDir(
      new File("./programs/invalid/semanticErr")
    )

    behavior of "compiler front-end"
    it should "verify syntax of valid programs" in {
        val syntaxFailed = ListBuffer[String]()

        assert(syntaxValid.nonEmpty)
        syntaxValid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(ast) => succeed
                case Failure(err) => {
                    syntaxFailed += x.getName()
                }
            }
        }
        if (syntaxFailed.length > 0) {
            fail(
              s"""syntax errors found in valid programs ${syntaxFailed
                  .toString()}""".stripMargin.replaceAll("\n", " ")
            )
        }
    }
    it should "verify semantics of valid programs" in {
        val semanticFailed = ListBuffer[String]()
        val log = ListBuffer[WaccError]()
        assert(syntaxValid.nonEmpty)
        syntaxValid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(ast) => {
                    log.clear()
                    ast.check(SymbolTable(), log)
                    if (log.length > 0) {
                        println(s"--------${x.getPath()}--------")
                        log.foreach { x => x.render() }
                        semanticFailed += x.getName()
                    }
                }
                case Failure(err) => {
                    fail(s"syntax errors found in valid program ${x.getName()}")
                }
            }
        }
        if (semanticFailed.length > 0) {
            println(s"THERE ARE ${semanticFailed.length} FAILED")
            fail(
              s"""semantic errors found in valid programs ${semanticFailed
                  .toString()}""".stripMargin.replaceAll("\n", " ")
            )
        } else {
            succeed
        }
    }

    it should "fail on semantically invalid programs" in {
        val syntaxFailed = ListBuffer[String]()
        val semanticPassed = ListBuffer[String]()

        assert(semanticInvalid.nonEmpty)
        semanticInvalid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(ast) => {
                    val errorBuffer = ListBuffer[WaccError]()
                    ast.check(SymbolTable(), errorBuffer)
                    val errorNum = errorBuffer.length
                    errorNum match {
                        case 0 => semanticPassed += x.getName()
                        case _ => succeed
                    }
                }
                case Failure(err) => syntaxFailed += x.getName()
            }
        }

        if (syntaxFailed.length > 0) {
            fail(
              s"""syntax errors found in syntactically valid 
              |programs ${syntaxFailed.toString()}""".stripMargin
                  .replaceAll("\n", " ")
            )
        }
        if (semanticPassed.length > 0) {
            fail(
              s"""semantic error should have been produced in 
                |programs ${semanticPassed
                  .toString()}""".stripMargin.replaceAll("\n", " ")
            )
        }
    }

    it should "fail on syntactically invalid programs" in {
        val syntaxPassed = ListBuffer[String]()
        assert(syntaxInvalid.nonEmpty)
        syntaxInvalid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(s) => {
                    syntaxPassed += x.getName()
                }
                case Failure(err) => succeed
            }
        }
        if (syntaxPassed.length > 0) {
            fail(
              s"""syntax error should have been produced in 
              |programs ${syntaxPassed.toString()}""".stripMargin
                  .replaceAll("\n", " ")
            )
        }
    }
}
