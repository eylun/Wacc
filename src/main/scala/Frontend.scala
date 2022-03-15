import java.io.File
import scala.util.Try
import parsley.io.{ParseFromIO}
import scala.collection.mutable.ListBuffer

/** Executes parser and generates code in the ARM representation */
object frontend {
    import parsley.{Success, Failure}
    import Helpers.cleanFilename

    val ArchLeadingChars = 7

    def main(args: Array[String]): Unit = {

        /** Check that only one argument is provided */
        assert(args.length == 2, "Usage: ./compile --arch=<arm/x86> <wacc filename>")
        val arch = args(0).substring(ArchLeadingChars)
        val fn = args(1)
        implicit val eb = new WaccErrorBuilder
        val waccFile = new File(fn)

        /** Parse the given .wacc file */
        val parseResult = syntax.parse.parseFromFile(waccFile).get
        parseResult match {
            case Success(result) =>
                val topLevelST = SymbolTable()
                val errorLog = ListBuffer[WaccError]()
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
