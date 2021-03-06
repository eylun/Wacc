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
    it should "execute uncaught exception errors" in {
        waccProgramsInDir(
          new File("./programs/valid/runtimeErr/uncaughtException")
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
    behavior of "try-catch programs"
    it should "execute try-catch statement errors" in {
        waccProgramsInDir(new File("./programs/valid/tryCatch"))
            .foreach((executeAndCompare(_)))
    }
    behavior of "map programs"
    it should "execute maps" in {
        waccProgramsInDir(new File("./programs/valid/higherOrderFunctions/map"))
            .foreach((executeAndCompare(_)))
    }
    behavior of "fold programs"
    it should "execute folds" in {
        waccProgramsInDir(new File("./programs/valid/higherOrderFunctions/fold"))
            .foreach((executeAndCompare(_)))
    }
    behavior of "scan programs"
    it should "execute scans" in {
        waccProgramsInDir(new File("./programs/valid/higherOrderFunctions/scan"))
            .foreach((executeAndCompare(_)))
    }
    behavior of "nested pairs"
    it should "execute statements with nested pairs" in {
        waccProgramsInDir(new File("./programs/valid/nestedPairs"))
            .foreach((executeAndCompare(_)))
    }
    behavior of "function overloading"
    it should "execute functions that are overloaded" in {
        waccProgramsInDir(new File("./programs/valid/functionOverload"))
            .foreach((executeAndCompare(_)))
    }
}
