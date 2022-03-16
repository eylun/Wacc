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
    def insertUtil(flag: UtilFlag)(implicit repr: Representation): Unit = {
        if (utilpool.contains(flag)) return
        utilpool += flag
        flag match {
            /** Utilities for print and println statements */
            case PPrintInt     => printIntLiter(this, repr)
            case PPrintBool    => printBoolLiter(this, repr)
            case PPrintString  => printStrLiter(this, repr)
            case PPrintRef     => printRef(this, repr)
            case PPrintNewLine => printNewLine(this, repr)
            /** utilities for read statements */
            case PReadChar => printReadChar(this, repr)
            case PReadInt  => printReadInt(this, repr)
            /** Utilities for free statements */
            case PFreePair => printFreePair(this, repr)
            /** Utilities for Errors and Checks */
            case PThrowOverflowError => printOverflowError(this, repr)
            case PRuntimeError       => printRuntimeError(this, repr)
            case PCheckDivideByZero  => printCheckDivideByZero(this, repr)
            case PCheckArrayBounds   => printCheckArrayBounds(this, repr)
            case PCheckNullPointer   => printCheckNullPointer(this, repr)
            case PExceptionError     => printExceptionError(this, repr)
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
    def emit()(implicit repr: Representation): List[Instruction] = toList(
      (dataMsgs.length, repr) match {
          case (0, ARMRepresentation) => bssMsgs ++: mainStatements ++: utilityStatements
          case (_, ARMRepresentation) =>
              bssMsgs ++: (Directive(
                "data"
              ) +=: dataMsgs) ++: mainStatements ++: utilityStatements

          case (0, X86Representation) => bssMsgs ++: (mainStatements.init :+ BranchLinkInstr("exit")) ++: utilityStatements
          case (_, X86Representation) =>
              bssMsgs ++: (Directive(
                "data"
              ) +=: dataMsgs) ++: (mainStatements.init :+ BranchLinkInstr("exit")) ++: utilityStatements
      }
    )
}
