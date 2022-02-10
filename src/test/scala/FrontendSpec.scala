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
    it should "get syntactically and semantically valid programs from folder" in {
        assert(syntaxValid.nonEmpty)
        syntaxValid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(ast) => {
                    ast.check(SymbolTable(), ListBuffer())
                    succeed
                }
                case Failure(err) => fail("Should have not failed")
            }
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
                    println(s"---------${x.getName()}---------")
                    errorBuffer.foreach(x => x.render())
                    errorNum match {
                        case 0 =>
                            fail(
                              s"${x.getName()} should have semantically failed"
                            )
                        case _ => succeed
                    }
                }
                case Failure(err) =>
                    fail("Should not have syntactically failed")
            }
        }
    }

    it should "get syntactically invalid programs from folder" in {
        assert(syntaxInvalid.nonEmpty)
        syntaxInvalid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(s) => {
                    fail("Invalid program somehow passed")
                }
                case Failure(err) => {
                    // println(s"---------${x.getName()}---------")
                    // err.render()
                    // println(err)
                    succeed
                }
            }
        }
    }
}
