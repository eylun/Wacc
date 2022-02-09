import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._
import java.io.File
class FrontendSpec extends AnyFlatSpec {
    import testUtils.{waccProgramsInDir, assertResultEquals}
    import parsley.{Failure, Success}
    import parsley.io.ParseFromIO

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
                    ast.check(SymbolTable())
                }
                case Failure(err) => fail(err)
            }
        }
    }

    it should "get invalid programs from folder" in {
        assert(syntaxInvalid.nonEmpty)
        syntaxInvalid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(s) => {
                    println(x)
                    fail("Invalid program somehow passed")
                }
                case Failure(err) => {
                    succeed
                }
            }
        }
    }
}
