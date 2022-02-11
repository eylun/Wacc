import java.io.File
import scala.util.Try
import parsley.io.{ParseFromIO}
import scala.collection.mutable.ListBuffer

object frontend {
    import parsley.{Success, Failure}
    def main(args: Array[String]): Unit = {
        assert(args.length == 1, "Usage: ./compile <wacc filename>")
        println("Parsing file: " + args(0))
        implicit val eb = new WaccErrorBuilder
        val waccFile = new File(args(0))
        val parseResult =
            syntax.parse.parseFromFile(waccFile)
        parseResult.get match {
            case Success(x) =>
                println(x)
                println(s"${args(0)} is syntactically valid")
                val topLevelST = SymbolTable()
                val errorLog = ListBuffer[WaccError]()
                x.check(topLevelST, errorLog)
                if (errorLog.length == 0) {
                    println(s"${args(0)} is semantically valid")
                    System.exit(0)
                }
                errorLog.foreach(e => e.render())
                System.exit(100)
            case Failure(err) =>
                err.render()
                System.exit(200)
        }
    }
}