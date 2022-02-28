import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import java.io.File

// case IntLiterNode(n) =>
// 	collector.addStatement(List(LoadImmIntInstr(Reg(0), n)))
// case CharLiterNode(c) =>
// 	collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(c))))
// case BoolLiterNode(true) =>
// 	collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(1))))
// case BoolLiterNode(false) =>
// 	collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(0))))

class ExecuteSpec extends AnyFlatSpec {
    import sys.process._
    import scala.language.postfixOps
    import parsley.{Success, Failure}
    import testUtils.{waccProgramsInDir, executeAndCompare}
    import Helpers.cleanFilename
    import java.io.PrintWriter
    import scala.io.Source

    if (!new File("/expected").exists()) {
        "mkdir -p expected" !
    }
    // behavior of "print programs"
    // it should "execute print programs" in {
    //     val printValid =
    //         waccProgramsInDir(new File("./programs/valid/IO/print"))
    //     "touch input.txt" !

    //     printValid.foreach(executeAndCompare(_))
    // }

    // behavior of "array programs"
    // it should "execute array programs" in {
    //     val arrayValid = waccProgramsInDir(new File("./programs/valid/array"))
    //     "touch input.txt" !

    //     arrayValid.foreach(executeAndCompare(_))
    // }
}
