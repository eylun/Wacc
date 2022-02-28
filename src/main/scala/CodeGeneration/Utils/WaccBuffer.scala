import scala.collection.mutable

class WaccBuffer {
    private var dataMsgCount = 0
    private var iteCount = 0
    private var wdCount = 0
    import constants._
    import Helpers._
    import Helpers.UtilFlag._

    /** One-off Utility Pool
      *
      * This set contains all the enums that currently exist within the program
      * If a enum already exists, it cannot be added a second time.
      */
    private val utilpool = mutable.Set[UtilFlag]().empty

    /** One-off Utility Function
      *
      * These functions are meant to add a label to the data messages. However
      * they should not occur more than once in a program, so calling them a
      * second time will not do anything
      *
      * On first call, the function will also add a list of instructions into
      * the utilities listbuffer which will be printed in the end
      */
    def insertUtil(flag: UtilFlag): Unit = {
        if (utilpool.contains(flag)) return
        utilpool += flag
        flag match {
            case PPrintInt            => printIntLiter(this)
            case PPrintBool           => printBoolLiter(this)
            case PPrintString         => printStrLiter(this)
            case PPrintRef            => printRef(this)
            case PPrintNewLine        => printNewLine(this)
            case PThrowOverflowError  =>
            case PRuntimeError        =>
            case PDivisionByZeroError =>
            case PCheckArrayBounds    =>
            case PReadChar            =>
            case PReadInt             =>
            case PFreePair            =>
            case PCheckNullPointer    =>
        }
    }

    private val dataMsgs: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction]().empty

    private val mainStatements: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction](
        )

    private val utilityStatements: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction]().empty

    def addUtilStatement(utilStatement: List[Instruction]): Unit = {
        utilityStatements ++= utilStatement
    }

    def setupMain(): Unit = {
        mainStatements ++= mainSetup
    }

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

    def toList(buffer: mutable.ListBuffer[Instruction]): List[Instruction] =
        buffer.toList

    def emit(): List[Instruction] = toList(
      dataMsgs.length match {
          case 0 => mainStatements ++ utilityStatements
          case _ =>
              (Directive(
                "data"
              ) +=: dataMsgs) ++ mainStatements ++ utilityStatements
      }
    )
}
