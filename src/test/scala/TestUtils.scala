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
            val currentWacc = current.filter(_.isFile).filter { 
                file => file.getName.endsWith(".wacc")
            }
            
            currentWacc ++ current.filter(_.isDirectory).flatMap(d => waccProgramsInDir(d))
        } else {
            Array[File]()
        }
    }

    /* Compares the expected Result object to the actual output produced by a 
       parse() call on a Parsley object. */
    def assertResultEquals[A](expected: Result[String, A], 
        actual: Result[String, A]): Unit = {
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
              "matched incorrect token (expected: " + expected + ", actual: " + actual + ")"
            )
    }

    /* Compares the expected identifier object and error log to the actual
       output produced by the check() function of an AST node. */
    def assertTypeIdEquals(expectedType: Option[Identifier], 
                            actualType: Option[Identifier], 
                            expectedLog: ListBuffer[WaccError],
                            actualLog: ListBuffer[WaccError]): Unit = {
        expectedLog.length match {
            // Expected to succeed
            case 0 => {
                actualType match {
                    case Some(t1) => {
                        expectedType match {
                            case Some(t2) => {
                                if (t1 != t2) 
                                    fail("incorrect type id (expected: " + t2 + ", actual: " + t1)
                            }
                            case None => {
                                fail("incorrect type id (expected: None, actual: " + t1)
                            }
                        }
                    }
                    case None => {
                        expectedType match {
                            case None => {}
                            case Some(t) => {
                                fail("incorrect type id (expected: " + t + ", actual: None")
                            }
                        }
                    }
                }
            }
            
            // Expected to fail
            case _ => {
                for (i <- 0 until expectedLog.length) {
                    if (i >= actualLog.length) {
                        fail("not enough errors produced (expected: " + expectedLog + ", actual: " + actualLog)
                    } else if (expectedLog(i) != actualLog(i)) {
                        fail(s"${i}th error did not match (expected: " + expectedLog + ", actual: " + actualLog)
                    }
                }
            }
        }  
    }
}
