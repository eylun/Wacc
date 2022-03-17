import scala.collection.mutable

class WaccBuffer {
    import constants._
    import Helpers._
    import Helpers.UtilFlag._

    /** Counters for data messages, if-then-else statements, do-while statements
      * and for the number of statements in general
      */
    private var generalCount = 0
    private var dataMsgCount = 0
    private var iteCount = 0
    private var wdCount = 0
    private var tryCatchCount = 0

    def tickTryCatch(): Int = {
        tryCatchCount += 1
        tryCatchCount - 1
    }

    def tickGeneral(): Int = {
        generalCount += 1
        generalCount - 1
    }

    def tickDataMsg(): Int = {
        dataMsgCount += 1
        dataMsgCount - 1
    }

    def tickIte(): Int = {
        iteCount += 1
        iteCount - 1
    }

    def tickWd(): Int = {
        wdCount += 1
        wdCount - 1
    }

    /** One-off Utility Pool
      *
      * This set contains all the enums that currently exist within the program
      * If a enum already exists, it cannot be added a second time.
      */
    private val utilpool = mutable.Set[UtilFlag]().empty

    /** One-off Utility Function
      *
      * These functions are meant to add data messages. However they should not
      * occur more than once in a program, so calling them a second time will
      * not do anything.
      *
      * On first call, the function will also add a list of instructions into
      * the utilities listbuffer which will be printed in the end
      */
    def insertUtil(flag: UtilFlag): Unit = {
        if (utilpool.contains(flag)) return
        utilpool += flag
        flag match {
            /** Utilities for print and println statements */
            case PPrintInt     => printIntLiter(this)
            case PPrintBool    => printBoolLiter(this)
            case PPrintString  => printStrLiter(this)
            case PPrintRef     => printRef(this)
            case PPrintNewLine => printNewLine(this)
            /** utilities for read statements */
            case PReadChar => printReadChar(this)
            case PReadInt  => printReadInt(this)
            /** Utilities for free statements */
            case PFreePair => printFreePair(this)
            /** Utilities for Errors and Checks */
            case PThrowOverflowError => printOverflowError(this)
            case PRuntimeError       => printRuntimeError(this)
            case PCheckDivideByZero  => printCheckDivideByZero(this)
            case PCheckArrayBounds   => printCheckArrayBounds(this)
            case PCheckStringBounds  => printCheckStringBounds(this)
            case PCheckNullPointer   => printCheckNullPointer(this)
            case PExceptionError     => printExceptionError(this)
        }
    }

    /** List of data messages */
    private val dataMsgs: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction]().empty

    def addDataMsg(msg: List[Instruction]): Unit = dataMsgs ++= msg

    private val bssMsgs: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction](
          Directive("bss"),
          Label("catch_address"),
          Directive("skip 4"),
          Label("prev_sp"),
          Directive("skip 4")
        )

    /** List of instructions in the 'main' instruction sequence */
    private val mainStatements: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction]().empty

    /** Adds '.text' and '.global main' directives */
    def setupMain(): Unit = {
        mainStatements ++= mainSetup
    }

    def addStatement(stat: List[Instruction]): Unit = mainStatements ++= stat

    def addStatement(stat: Instruction): Unit = mainStatements += stat

    /** List of utility instruction statements */
    private val utilityStatements: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction]().empty

    def addUtilStatement(utilStatement: List[Instruction]): Unit = {
        utilityStatements ++= utilStatement
    }

    def toList(buffer: mutable.ListBuffer[Instruction]): List[Instruction] =
        buffer.toList

    /** emit() concatenates the data messages (if any) together with the
      * instructions generated for the main statements and utility statements
      */
    def emit(): List[Instruction] = toList(
      dataMsgs.length match {
          case 0 => bssMsgs ++: mainStatements ++: utilityStatements
          case _ =>
              bssMsgs ++: (Directive(
                "data"
              ) +=: dataMsgs) ++: mainStatements ++: utilityStatements
      }
    )
}
