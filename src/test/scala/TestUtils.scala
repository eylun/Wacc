import org.scalatest.matchers.should.Matchers._
import java.io.File
import parsley.Parsley, Parsley._
import scala.collection.mutable.ListBuffer

object testUtils {
    import parsley.{Result, Success, Failure}

    /* Returns an array of files with the .wacc extension in a directory. */
    def waccProgramsInDir(dir: File): Array[File] = {
        if (dir.exists && dir.isDirectory) {
            val current = dir.listFiles
            val currentWacc = current.filter(_.isFile).filter { file =>
                file.getName.endsWith(".wacc")
            }

            currentWacc ++ current
                .filter(_.isDirectory)
                .flatMap(d => waccProgramsInDir(d))
        } else {
            Array[File]()
        }
    }

    /* Compares the expected List of Instructions to the actual output inserted into a
		WaccBuffer during code generation */
    def assertCodegenEquals(
        expected: List[Instruction],
        actual: List[Instruction]
    ): Unit = {
        if (expected != actual)
            fail(
              s"Instructions do not match\nexpected: $expected, actual: $actual."
            )
    }

    /* Compares the expected output string and actual output string */
    def assertExecuteEquals(expected: String, actual: String): Unit = {
        (expected, actual) match {
            case (s"${exFront}0x$_ $exBack", s"${actFront}0x$_ $actBack")
                if exFront != actFront || exBack != exBack =>
                fail(s"Expected: $expected, Actual: $actual")
            case (_, _) if expected != actual =>
                fail(s"Expected: $expected, Actual: $actual")

        }
    }

    /* Compares the expected Result object to the actual output produced by a
       parse() call on a Parsley object. */
    def assertResultEquals[A](
        expected: Result[String, A],
        actual: Result[String, A]
    ): Unit = {
        expected match {
            case Success(x) => {
                actual match {
                    case Success(y)   => checkTokenMatch(x, y)
                    case Failure(err) => fail(err)
                }
            }
            case Failure(_) => {
                actual match {
                    case Success(s) =>
                        fail("did not fail as expected, actual: " + s)
                    case Failure(_) => {}
                }
            }
        }
    }

    def checkTokenMatch[A](expected: A, actual: A): Unit = {
        if (expected != actual)
            fail(
              s"""matched incorrect token (expected: ${expected}, 
              |actual: ${actual})""".stripMargin.replaceAll("\n", "")
            )
    }

    /* Compares the expected identifier object and error log to the actual
       output produced by the check() function of an AST node. */
    def assertTypeIdEquals(
        expectedType: Option[Identifier],
        actualType: Option[Identifier],
        expectedLog: ListBuffer[WaccError],
        actualLog: ListBuffer[WaccError]
    ): Unit = {
        expectedLog.length match {
            // Expected to succeed
            case 0 => {
                actualType match {
                    case Some(t1) => {
                        expectedType match {
                            case Some(t2) => {
                                if (t1 != t2)
                                    fail(s"""incorrect type id (expected: ${t2},
                                        | actual: ${t1}
                                        |""".stripMargin.replaceAll("\n", ""))
                            }
                            case None => {
                                fail(s"""incorrect type id (expected: None, 
                                        |actual: ${t1}
                                        |""".stripMargin.replaceAll("\n", ""))
                            }
                        }
                    }
                    case None => {
                        expectedType match {
                            case None => {}
                            case Some(t) => {
                                fail(s"""incorrect type id (expected: ${t}, 
                                        |actual: None
                                        |""".stripMargin.replaceAll("\n", ""))
                            }
                        }
                    }
                }
            }

            // Expected to fail
            case _ => {
                for (i <- 0 until expectedLog.length) {
                    if (i >= actualLog.length) {
                        fail(s"""not enough errors produced (expected: 
                            |${expectedLog}, actual: 
                            |${actualLog}""".stripMargin.replaceAll("\n", ""))
                    } else if (expectedLog(i) != actualLog(i)) {
                        fail(s"""${i}th error did not match (expected: 
                            |${expectedLog}, actual: 
                            |${actualLog}""".stripMargin.replaceAll("\n", ""))
                    }
                }
            }
        }
    }

    /** Generate assembly file through test suite
      *
      * Assumptions are made here that there are no syntax errors and no
      * semantic errors
      */
    def testCodegen(f: File): Unit = {
        import parsley.io.{ParseFromIO}
        import Helpers.cleanFilename
        val result = syntax.parse.parseFromFile(f).get
        val topLevelST = SymbolTable()
        val errorLog = ListBuffer[WaccError]()
        result.get.check(topLevelST, errorLog)
        ARMRepresentation(
          result.get,
          topLevelST,
          cleanFilename(f.getPath()) + ".s"
        )
    }

    def executeAndCompare(f: File): Unit = {

        import sys.process._
        import scala.language.postfixOps
        import parsley.{Success, Failure}
        import Helpers.cleanFilename
        import java.io.{
            OutputStream,
            ByteArrayOutputStream,
            InputStream,
            ByteArrayInputStream
        }
        import regexHelper._

        println(s"---${f.getName()}---")
        this.testCodegen(f)

        s"arm-linux-gnueabi-gcc -o ${cleanFilename(f.getPath())} -mcpu=arm1176jzf-s -mtune=arm1176jzf-s ${cleanFilename(f.getPath())}.s" !

        val (input, expectedOutput, expectedExit) = extractTest(f)

        val inputStream: InputStream = new ByteArrayInputStream(
          input.getBytes()
        )
        val outputStream: OutputStream = new ByteArrayOutputStream()
        val actualExit =
            s"qemu-arm -L /usr/arm-linux-gnueabi/ ${cleanFilename(f.getPath())}" #< inputStream #> outputStream !

        val actualOutput = outputStream.toString().trim()
        println("actual input")
        println(input)
        println("actual exit code")
        println(actualExit)
        println("actual output")
        println(actualOutput)
        println("extracted from wacc file:")
        println(s"expected input: $input")
        println(s"expected output: $expectedOutput")
        println(s"expected exit: $expectedExit")

        // s"rm ${cleanFilename(f.getPath())}.s" !

        s"rm ${cleanFilename(f.getPath())}" !

        inputStream.reset()

        (expectedOutput.split("\n") zip actualOutput.split("\n")).foreach {
            case (expectedAddrRegex(el, er), actualAddrRegex(al, _, ar))
                if el == al && er == ar =>
            case (expectedRuntimeErrRegex, actualRuntimeErrRegex(_*)) =>
            case (e, a) if e == a                                     =>
            case (e, a) =>
                fail(
                  s"${f.getName()}\nExpected Output : [$e]\nActual Output   : [$a]"
                )

        }
        if (expectedExit != actualExit)
            fail(
              s"${f.getName()}\nExpected Exit : [$expectedExit]\nActual Exit   : [$actualExit]"
            )
    }

    def extractTest(f: File): (String, String, Int) = {
        import scala.io.Source
        Source.fromFile(f.getPath()).getLines().mkString("\n") match {
            case s"$_# Input:$input# Output:$output# Exit:$exit# Program$_" =>
                (
                  extractInputOutput(input.trim()),
                  extractInputOutput(output.trim()),
                  exit.trim().toInt
                )
        }
    }

    def extractInputOutput(str: String): String = {
        println(s"STRING INPUT OUTPUT${str
            .split("\n")
            .map {
                case regexHelper.startRegex(str) => str
                case _                           => ""
            }
            .mkString}")
        str.split("\n")
            .map {
                case regexHelper.startRegex(str) => str
                case _                           => ""
            }
            .mkString("\n")
    }

    object regexHelper {
        val expectedAddrRegex = raw"(.*)#addrs#(.*)".r
        val actualAddrRegex = raw"(.*)0x([0-9a-fA-F]+)(.*)".r

        val expectedRuntimeErrRegex = raw"#runtime_error#".r
        val actualRuntimeErrRegex = raw"(.*)Error(.*)".r

        val startRegex = raw"# (.*)".r
    }
}
