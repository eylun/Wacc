import scala.collection.mutable
import constants._

class WaccBuffer {
    private var dataMsgCount = 0
    private var iteCount = 0
    private var wdCount = 0

    private val dataMsgs: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction]().empty

    private val mainStatements: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction](
          Directive("text"),
          Directive("global main"),
          Label("main"),
          PushInstr(List(lr))
        )

    private val functions: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction]().empty

    def tickDataMsg(): Int = {
        dataMsgCount += 1
        dataMsgCount - 1
    }

    def addDataMsg(msg: List[Instruction]): Unit = dataMsgs ++= msg

    def tickIte(): Int = {
        iteCount += 1
        iteCount - 1
    }

    def addStatement(stat: List[Instruction]): Unit = mainStatements ++= stat

    def tickWd(): Int = {
        wdCount += 1
        wdCount - 1
    }

    def addFunc(func: List[Instruction]): Unit = functions ++= func

    def toList(buffer: mutable.ListBuffer[Instruction]): List[Instruction] =
        buffer.toList

    def emit(): List[Instruction] = toList(
      dataMsgs ++ functions ++ mainStatements
    )
}
