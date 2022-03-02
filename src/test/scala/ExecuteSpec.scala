import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import java.io.File
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
    behavior of "print programs"
    it should "execute print programs" in {
        waccProgramsInDir(new File("./programs/valid/IO/print"))
            .foreach(executeAndCompare(_))
    }

    behavior of "array programs"
    it should "execute array programs" in {
        waccProgramsInDir(new File("./programs/valid/array"))
            .foreach(executeAndCompare(_))
    }

    behavior of "basic programs"
    it should "execute skip statements" in {
        waccProgramsInDir(new File("./programs/valid/basic/skip"))
            .foreach(executeAndCompare(_))
    }

    it should "execute exit statements" in {
        waccProgramsInDir(new File("./programs/valid/basic/exit"))
            .foreach(executeAndCompare(_))
    }
    behavior of "expression programs"
    it should "execute expressions" in {
        waccProgramsInDir(new File("./programs/valid/expressions"))
            .foreach(executeAndCompare(_))
    }
    behavior of "function programs"
    it should "execute simple functions" in {
        waccProgramsInDir(
          new File("./programs/valid/function/simple_functions")
        )
            .foreach(executeAndCompare(_))
    }
    // it should "execute nested functions" in {}
    // behavior of "if programs"
    // behavior of "read programs"
    // behavior of "pair programs"
    // behavior of "runtime error programs"
    // it should "execute and return array out of bounds" in {}
    // it should "execute and return division by zero" in {}
    // it should "execute and return integer overflow" in {}
    // it should "execute and return null dereference" in {}
    // behavior of "basic programs"
    // behavior of "basic programs"
}
