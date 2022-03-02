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
              s"Instructions do not match\nexpected: $expected\nactual: $actual."
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

        println(s"---${f.getName()}---")
        this.testCodegen(f)

        s"arm-linux-gnueabi-gcc -o ${cleanFilename(f.getPath())} -mcpu=arm1176jzf-s -mtune=arm1176jzf-s ${cleanFilename(f.getPath())}.s" !

        val (input, expectedOutput, expectedExit) = extractTest(f)

        val inputStream: InputStream = new ByteArrayInputStream(
          expectedOutput.getBytes()
        )
        val outputStream: OutputStream = new ByteArrayOutputStream()
        val actualExit =
            s"qemu-arm -L /usr/arm-linux-gnueabi/ ${cleanFilename(f.getPath())}" #< inputStream #> outputStream !

        val actualOutput = outputStream.toString().trim()

        println(actualExit)
        println(actualOutput)
        println("extracted from wacc file:")
        println(s"input: $input")
        println(s"output: $expectedOutput")
        println(s"exit: $expectedExit")

        s"rm ${cleanFilename(f.getPath())}.s" !

        s"rm ${cleanFilename(f.getPath())}" !

        if (expectedOutput != actualOutput)
            fail(
              s"Expected Output: [$expectedOutput]\nActual Output: [$actualOutput]"
            )
        if (expectedExit != actualExit)
            fail(s"Expected Exit: [$expectedExit]\nActual Exit: [$actualExit]")
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
        println(
          str.split("\n")
              .map(s =>
                  s.length match {
                      case 0 => ""
                      case _ => s.substring(2)
                  }
              )
              .toList
        )
        str.split("\n")
            .map(s =>
                s.length match {
                    case 0 => ""
                    case _ => s.substring(2)
                }
            )
            .mkString("\n")
    }
}
