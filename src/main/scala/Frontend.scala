import java.io.File
import scala.util.Try
import parsley.io.{ParseFromIO}
import scala.collection.mutable.ListBuffer
import OptimisationFlag._

/** Executes parser and generates code in the ARM representation */
object frontend {
    import parsley.{Success, Failure}
    import Helpers.cleanFilename

    val ArchLeadingChars = 7

    def main(args: Array[String]): Unit = {

        /** Check that only one argument is provided */
        assert(
          args.length == 1 ||
              (args.length == 2) ||
              (args.length == 3 && OptimisationFlag.allOptFlags.contains(args(2))),
          "Usage: ./compile --arch=<arm/x86> <wacc filename> -O<level> "
        )
        var fn = ""
        var arch = ""
        if (args.length > 1) {
            arch = args(0).substring(ArchLeadingChars)
            fn = args(1)
        } else {
            fn = args(0)
            arch = "arm"
        }
        implicit val eb = new WaccErrorBuilder
        val waccFile = new File(fn)

        var optFlag = OptimisationFlag.O0
        if (args.length == 3) {
            optFlag = OptimisationFlag.withName(args(2).substring(1))
        }

        /** Parse the given .wacc file */
        val parseResult = syntax.parse.parseFromFile(waccFile).get
        parseResult match {
            case Success(result) =>
                val topLevelST = SymbolTable()
                val errorLog = ListBuffer[WaccError]()
                var length = errorLog.length
                result.check(topLevelST, errorLog)
                if (errorLog.length == 0) {

                    /** No syntax errors, move on to code generation */
                    arch match {
                        case "arm" => {
                            ARMRepresentation(result, topLevelST, cleanFilename(waccFile.getName()) + ".s")
                        }
                        case "x86" => {
                            X86Representation(result, topLevelST, cleanFilename(waccFile.getName()) + ".s")
                        }
                    }
                    System.exit(0)
                }

                /** SEMANTIC ERROR */
                errorLog.foreach(e => e.render())
                System.exit(200)
            case Failure(err) =>
                /** SYNTAX ERROR */
                err.render()
                System.exit(100)
        }

    }
}
