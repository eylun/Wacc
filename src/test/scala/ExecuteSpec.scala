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
    import testUtils.{waccProgramsInDir, testCodegen, outputIndicator}
    import Helpers.cleanFilename
    import java.io.PrintWriter
    import scala.io.Source

    if (!new File("/expected").exists()) {
        "mkdir -p expected" !
    }
    behavior of "print programs"
    it should "execute print programs" in {
        val syntaxValid =
            waccProgramsInDir(new File("./programs/valid/IO/print"))
        "touch input.txt" !

        syntaxValid.foreach { f =>
            if (!new File(s"expected/${cleanFilename(f.getName())}").exists()) {
                println("Caching outputs...")
                // s"touch expected/${cleanFilename(f.getName())}" !

                val output = (s"./refCompile -x ${f.getPath()}" #< new File(
                  "input.txt"
                )) !!

                println("-----------------")
                output match {
                    case s"$_===========================================================$o===$_" => {
                        new PrintWriter(
                          s"expected/${cleanFilename(f.getName())}"
                          /** Left trim */
                        ) { write(o.replaceAll("^\\s+", "")); close }
                    }
                    case _ =>
                        new PrintWriter(
                          s"expected/${cleanFilename(f.getName())}"
                        ) { write(""); close }
                }
            }
            testCodegen(f)

            s"arm-linux-gnueabi-gcc -o ${cleanFilename(f.getPath())} -mcpu=arm1176jzf-s -mtune=arm1176jzf-s ${cleanFilename(f.getPath())}.s" !

            val actual =
                (s"qemu-arm -L /usr/arm-linux-gnueabi/ ${cleanFilename(f.getPath())} < input.txt" !!).trim()

            val expected =
                Source
                    .fromFile(s"expected/${cleanFilename(f.getName())}")
                    .getLines()
                    .mkString("\n")
            s"rm ${cleanFilename(f.getPath())}.s" !

            s"rm ${cleanFilename(f.getPath())}" !

            if (expected == actual) succeed
            else fail(s"Expected: $expected, Actual: $actual")
        }
        "rm input.txt" !

    }
}
