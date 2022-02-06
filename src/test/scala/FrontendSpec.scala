import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._
import java.io.File

class FrontendSpec extends AnyFlatSpec {
    import testUtils.{waccProgramsInDir}

    // TODO: load programs in batches with more specific test messages
    val valid = waccProgramsInDir(new File("./programs/valid"))
    val invalid = waccProgramsInDir(new File("./programs/invalid/syntaxErr"))

    behavior of "compiler front-end"
    it should "get valid programs from folder" in {
        assert(valid.nonEmpty)
    }

    it should "get invalid programs from folder" in {
        assert(invalid.nonEmpty)
    }
}
