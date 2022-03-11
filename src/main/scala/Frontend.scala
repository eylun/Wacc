import java.io.File
import scala.util.Try
import parsley.io.{ParseFromIO}
import scala.collection.mutable.ListBuffer

/** Executes parser and generates code in the ARM representation */
object frontend {
    import parsley.{Success, Failure}
    import Helpers.cleanFilename
    def main(args: Array[String]): Unit = {

        /** Check that only one argument is provided */
        assert(args.length == 1, "Usage: ./compile <wacc filename>")
        val fn = args(0)
        implicit val eb = new WaccErrorBuilder
        val waccFile = new File(fn)
        println("after assertion")
        /** Parse the given .wacc file */
        val parseResult = syntax.parse.parseFromFile(waccFile).get
        println("before match")
        parseResult match {
            case Success(result) =>
                val topLevelST = SymbolTable()
                val errorLog = ListBuffer[WaccError]()
                println("before check")
                var length = errorLog.length
                result.check(topLevelST, errorLog)
                length = errorLog.length
                if (errorLog.length == 0) {
                    println("successful")
                    /** No syntax errors, move on to code generation */
                    ARMRepresentation(
                      result,
                      topLevelST,
                      cleanFilename(waccFile.getName()) + ".s"
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
