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

    /* Compares the output of an executed program and the expected output */
    def assertExecuteEquals(
        expected: List[String],
        actual: List[String]
    ): Unit = {
        if (expected.length != actual.length)
            fail("Expected and Actual values have varying lengths")
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

    /** Extracts the output segment of a wacc file provided in the test cases */
    def extractOutput(f: File): List[String] = {
        import scala.io.Source
        import scala.collection.mutable
        val result: mutable.ListBuffer[String] =
            mutable.ListBuffer[String]().empty
        var addFlag = false
        Source.fromFile(f.getPath()).getLines().foreach { line =>
            line match {
                case "# Output:"             => addFlag = true
                case "# Input:"              => addFlag = false
                case s"# $output" if addFlag => result += output
                case _                       => addFlag = false
            }
        }
        result.toList
    }
}
