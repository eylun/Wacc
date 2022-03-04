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
    it should "execute nested functions" in {
        waccProgramsInDir(
          new File("./programs/valid/function/nested_functions")
        )
            .foreach(executeAndCompare(_))
    }
    behavior of "if programs"
    it should "execute if statements" in {
        waccProgramsInDir(new File("./programs/valid/if"))
            .foreach((executeAndCompare(_)))
    }
    behavior of "read programs"
    it should "execute read statements" in {
        waccProgramsInDir(
          new File("./programs/valid/IO/read")
        )
            .foreach(executeAndCompare(_))
    }
    behavior of "IO programs"
    it should "execute IO statements" in {
        waccProgramsInDir(
          new File("./programs/valid/IO")
        )
            .foreach(executeAndCompare(_))
    }
    behavior of "pair programs"
    it should "execute pair programs" in {
        waccProgramsInDir(new File("./programs/valid/pairs"))
            .foreach(executeAndCompare(_))
    }
    behavior of "runtime error programs"
    it should "execute array out of bounds errors" in {
        waccProgramsInDir(
          new File("./programs/valid/runtimeErr/arrayOutOfBounds")
        )
            .foreach((executeAndCompare(_)))
    }
    it should "execute divide by zero errors" in {
        waccProgramsInDir(new File("./programs/valid/runtimeErr/divideByZero"))
            .foreach((executeAndCompare(_)))
    }
    it should "execute integer overflow errors" in {
        waccProgramsInDir(
          new File("./programs/valid/runtimeErr/integerOverflow")
        )
            .foreach((executeAndCompare(_)))
    }
    it should "execute null dereference errors" in {
        waccProgramsInDir(
          new File("./programs/valid/runtimeErr/nullDereference")
        )
            .foreach((executeAndCompare(_)))
    }
    behavior of "scope programs"
    it should "execute scope statements" in {
        waccProgramsInDir(new File("./programs/valid/scope"))
            .foreach((executeAndCompare(_)))
    }
    behavior of "sequence programs"
    it should "execute sequence programs" in {
        waccProgramsInDir(new File("./programs/valid/sequence"))
            .foreach((executeAndCompare(_)))
    }
    behavior of "variable programs"
    it should "execute variable errors" in {
        waccProgramsInDir(new File("./programs/valid/variables"))
            .foreach((executeAndCompare(_)))
    }
    behavior of "while programs"
    it should "execute while statement errors" in {
        waccProgramsInDir(new File("./programs/valid/while"))
            .foreach((executeAndCompare(_)))
    }
}
