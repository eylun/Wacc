import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._
import java.io.File
class FrontendSpec extends AnyFlatSpec {
    import testUtils.{waccProgramsInDir, assertResultEquals}
    import parsley.{Failure, Success}
    import parsley.io.ParseFromIO

    // TODO: load programs in batches with more specific test messages
    val valid = waccProgramsInDir(new File("./programs/valid"))
    val invalid = waccProgramsInDir(new File("./programs/invalid/syntaxErr"))

    behavior of "compiler front-end"
    it should "get valid programs from folder" in {
        assert(valid.nonEmpty)
        valid.foreach { case x: File =>
            syntax.parse.parseFromFile(x).get match {
                case Success(_)   => succeed
                case Failure(err) => fail(err)
            }
        }
    }

    it should "get invalid programs from folder" in {
        assert(invalid.nonEmpty)
        invalid.foreach { case x: File =>
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
