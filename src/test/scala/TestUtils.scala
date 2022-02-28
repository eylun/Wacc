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
        import java.io.PrintWriter
        import scala.io.Source
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
        this.testCodegen(f)

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

        "rm input.txt" !
    }
}
