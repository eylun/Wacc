import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._
import java.io.File

object utils {
    def waccProgramsInDir(dir: File): Array[File] = {
        if (dir.exists && dir.isDirectory) {
            val current = dir.listFiles
            val currentWacc = current.filter(_.isFile).filter { 
                file => file.getName.endsWith(".wacc")
            }
            
            currentWacc ++ current.filter(_.isDirectory).flatMap(d => waccProgramsInDir(d))
        } else {
            Array[File]()
        }
    }
}

class ParserSpec extends AnyFlatSpec {
    // TODO: load programs in batches with more specific test messages
    val valid = utils.waccProgramsInDir(new File("./programs/valid"))
    val invalid = utils.waccProgramsInDir(new File("./programs/invalid/syntaxErr"))

    behavior of "Parser"
    it should "get valid programs from folder" in {
        assert(valid.nonEmpty)
    }

    it should "get invalid programs from folder" in {
        assert(invalid.nonEmpty)
    }
}
