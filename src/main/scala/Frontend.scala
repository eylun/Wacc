import java.io.File
import scala.util.Try
import parsley.io.{ParseFromIO}
import scala.collection.mutable.ListBuffer
import OptimisationFlag._

/** Executes parser and generates code in the ARM representation */
object frontend {
    import parsley.{Success, Failure}
    import Helpers.cleanFilename
    def main(args: Array[String]): Unit = {

        /** Check that only one argument is provided */
        assert(args.length == 1 || (args.length == 2 && OptimisationFlag.allOptFlags.contains(args(1))), "Usage: ./compile <wacc filename> -O<level>")
        val fn = args(0)
        implicit val eb = new WaccErrorBuilder
        val waccFile = new File(fn)

        var optFlag = OptimisationFlag.O0
        if(args.length == 2){
            optFlag = OptimisationFlag.withName(args(1).substring(1))
        }

        /** Parse the given .wacc file */
        val parseResult = syntax.parse.parseFromFile(waccFile).get
        parseResult match {
            case Success(result) =>
                val topLevelST = SymbolTable()
                val errorLog = ListBuffer[WaccError]()
                result.check(topLevelST, errorLog)
                if (errorLog.length == 0) {

                    /** No syntax errors, move on to code generation */
                    ARMRepresentation(
                      result,
                      topLevelST,
                      cleanFilename(waccFile.getName()) + ".s", 
                      optFlag
                    )
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
