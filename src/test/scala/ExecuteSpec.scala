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
    import testUtils.{waccProgramsInDir, extractOutput}
    import Helpers.cleanFilename
    behavior of "array programs"
    it should "execute aray programs" in {
        val syntaxValid =
            waccProgramsInDir(new File("./programs/valid/IO/print"))
        "touch input.txt" !

        syntaxValid.foreach { f =>
            val output = extractOutput(f)
            // println(output)
            s"./compile ${f.getPath()}" !

            s"arm-linux-gnueabi-gcc -o ${cleanFilename(f.getPath())} -mcpu=arm1176jzf-s -mtune=arm1176jzf-s ${cleanFilename(f.getPath())}.s" !

            val result =
                s"qemu-arm -L /usr/arm-linux-gnueabi/ ${cleanFilename(f.getPath())} < input.txt" !!

            println(s"HAHAHAHA $result")
        }

    }
}
