import java.io.File
import scala.util.Try
import parsley.io.{ParseFromIO}

object SyntaxParser {
    import parsley.{Success, Failure}
    def main(args: Array[String]): Unit = {
        assert(args.length == 1, "Usage: ./compile <wacc filename>")
        println("Parsing file: " + args(0))
        // implicit val eb = new WaccErrorBuilder
        val waccFile = new File(args(0))
        val parseResult =
            syntax.parse.parseFromFile(waccFile)
        parseResult.get match {
            case Success(x) =>
                println(x)
                println(s"${args(0)} is synctactically valid.")
                val topLevelST = SymbolTable()
                val errorLog = List()
                x.check(topLevelST, errorLog)
                if (errorLog.length == 0) {
                    System.exit(0)
                }
                println(errorLog)
                System.exit(100)
            case Failure(err) =>
                println("Syntax Error")
                println(err)
                System.exit(200)
        }
    }
}
