import org.scalatest.matchers.should.Matchers._
import java.io.File
import parsley.Parsley, Parsley._

object testUtils {
    import parsley.{Result, Success, Failure}
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

    def assertResultEquals[A](expected: Result[String, A], 
        actual: Result[String, A]) = {
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
                        case Failure(_) => succeed
                    }
                }
            }
    }

    def checkTokenMatch[A](expected: A, actual: A) = {
        if (expected == actual) succeed
        else
            fail(
              "matched incorrect token (expected: " + expected + ", actual: " + actual + ")"
            )
    }
}
