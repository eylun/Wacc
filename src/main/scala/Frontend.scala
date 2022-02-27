import java.io.File
import scala.util.Try
import parsley.io.{ParseFromIO}
import scala.collection.mutable.ListBuffer

object frontend {
    import parsley.{Success, Failure}
    import Helpers.cleanFilename
    def main(args: Array[String]): Unit = {
        assert(args.length == 1, "Usage: ./compile <wacc filename>")
        val fn = args(0)
        println("Parsing file: " + fn)
        implicit val eb = new WaccErrorBuilder
        val waccFile = new File(fn)
        val parseResult = syntax.parse.parseFromFile(waccFile).get
        parseResult match {
            case Success(result) =>
                println(s"$fn is syntactically valid")
                val topLevelST = SymbolTable()
                val errorLog = ListBuffer[WaccError]()
                result.check(topLevelST, errorLog)
                if (errorLog.length == 0) {
                    println(s"$fn is semantically valid")

                    /** Code Generation */
                    ARMRepresentation(
                      result,
                      topLevelST,
                      cleanFilename(fn) + ".s"
                    )
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
