import scala.collection.mutable
import constants._
import Helpers.UtilFlag._
import parsley.internal.machine.instructions.Pop

class WaccBuffer {
    private var dataMsgCount = 0
    private var iteCount = 0
    private var wdCount = 0

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
            case PPrintString => {
                val message = s"msg_${tickDataMsg()}"
                utilityStatements ++= List(
                  Label(s"p_print_string"),
                  PushInstr(List(lr)),
                  LoadInstr(r1, r0, ImmOffset(0)),
                  AddInstr(r2, r0, ImmOffset(4)),
                  LoadImmLabelInstr(r0, message),
                  AddInstr(r0, r0, ImmOffset(4)),
                  BranchLinkInstr("printf", Condition.AL),
                  MoveInstr(r0, ImmOffset(0)),
                  BranchLinkInstr("fflush", Condition.AL),
                  PopInstr(List(pc))
                ) // Put this code into helpers
                dataMsgs ++= List() // Data Msgs
            }
            case PPrintLn             => //
            case PPrintInt            => //
            case PPrintRef            => //
            case PThrowOverflowError  => //
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
          Directive("text"),
          Directive("global main"),
          Label("main"),
          PushInstr(List(lr))
        )

    private val functions: mutable.ListBuffer[Instruction] =
        mutable.ListBuffer[Instruction]().empty

    private val utilityStatements: mutable.ListBuffer[Instruction] =
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
      dataMsgs ++ functions ++ mainStatements ++ utilityStatements
    )
}
