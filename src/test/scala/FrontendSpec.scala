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

    // TODO: load programs in batches with more specific test messages
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
            fail(s"semantic errors found in valid programs ${syntaxFailed.toString()}")
        }
    }
    it should "verify semantics of valid programs" in {
        val semanticFailed = ListBuffer[String]()

        assert(syntaxValid.nonEmpty)
        syntaxValid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(ast) => {
                    val log = ListBuffer[WaccError]()
                    ast.check(SymbolTable(), log)
                    if (log.length > 0) {
                        semanticFailed += x.getName()
                    }
                }
                case Failure(err) => {
                    fail(s"syntax errors found in valid program ${x.getName()}")
                }
            }
        }
        if (semanticFailed.length > 0) {
            fail(s"semantic errors found in valid programs ${semanticFailed.toString()}")
        }
    }

    it should "get syntactically valid but semantically invalid programs from folder" in {
        assert(semanticInvalid.nonEmpty)
        semanticInvalid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(ast) => {
                    val errorBuffer = ListBuffer[WaccError]()
                    ast.check(SymbolTable(), errorBuffer)
                    val errorNum = errorBuffer.length
                    errorNum match {
                        case 0 =>
                            fail(
                              s"${x.getName()} should have semantically failed"
                            )
                        case _ => succeed
                    }
                }
                case Failure(err) =>
                    fail(s"${x.getName()} should not have syntactically failed")
            }
        }
    }

    it should "get syntactically invalid programs from folder" in {
        assert(syntaxInvalid.nonEmpty)
        syntaxInvalid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(s) => {
                    fail(s"${x.getName()} should have syntactically failed")
                }
                case Failure(err) => {
                    succeed
                }
            }
        }
    }
}
