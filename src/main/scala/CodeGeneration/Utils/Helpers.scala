import Condition._
import constants._
import transExpression._

object Helpers {
    val WORD_SIZE = 4
    val BIT_SIZE = 1

    val mainSetup = List(
      Directive("text"),
      Directive("global main"),
      Label("main"),
      PushInstr(List(lr))
    )

    def getTypeSize(t: Identifier): Int = {
        t match {
            case BoolType() | CharType() => BIT_SIZE
            case Variable(t)             => getTypeSize(t)
            /** Functions are not applicable for stack frame sizes */
            case FunctionId(_, _, _) => 0
            case _                   => WORD_SIZE
        }
    }

    def getStringDirective(s: String, idx: Int): List[Instruction] = {

        List(
          Label(s"msg_$idx:"),
          Directive(s".word ${s.length()}"),
          Directive(s".ascii \"$s\"")
        )
    }

    def getArraySize(t: TypeNode, size: Int): Int = {
        t match {
            case at @ ArrayTypeNode(elemType, dimension) => {
                dimension match {
                    case 1 => getTypeSize(elemType.typeId.get) * size
                    case _ => 4 * size
                }
            }

            // cannot be called on other types
            case _ => -1
        }
    }

    def addNewPairElem(e: ExprNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        transExpression(e, stackFrame)
        val t = e.typeId.get
        collector.addStatement(
          List(
            PushInstr(List(Reg(0))),
            MoveInstr(Reg(0), ImmOffset(getTypeSize(t))),
            BranchLinkInstr("malloc", AL),
            PopInstr(List(Reg(1)))
          )
        )
        t match {
            case BoolType() | CharType() =>
                collector.addStatement(
                  List(StoreByteInstr(Reg(1), Reg(0), ImmOffset(0)))
                )
            case _ =>
                collector.addStatement(
                  List(StoreInstr(Reg(1), Reg(0), ImmOffset(0)))
                )
        }
        collector.addStatement(List(PushInstr(List(Reg(0)))))
    }

    /** PRINT STATEMENT HELPERS */
    /** Print Int Literal */
    def getPrintIntDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx:"),
          Directive(s".word 3"),
          Directive(s".ascii \"%d\\0\"")
        )
    }

    def printIntLiterFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_print_int"),
          PushInstr(lr),
          MoveInstr(Reg(1), RegOp(Reg(0))),
          LoadLabelInstr(Reg(0), s"msg_$idx:"),
          AddInstr(Reg(0), Reg(0), ImmOffset(4)),
          BranchLinkInstr("printf"),
          MoveInstr(Reg(0), ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(pc)
        )
    }

    def printIntLiter() = {

        /** Add DataMsg for the Int literal */
        Int idx = collector.tickDataMsg()
        collector.addDataMsg(getPrintIntDirective(idx))

        /** Add p_print_int function */
        collector.addFunc(
          printIntLiterFunc(idx)
        )

        /** Add branch instruction Statement */
        collector.addStatement(
          List(BranchLinkInstr("p_print_int"))
        )
    }

    /** Print Bool Literal */
    def getPrintTrueDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx:"),
          Directive(s".word 5"),
          Directive(s".ascii \"true\\0\"")
        )
    }

    def getPrintFalseDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx:"),
          Directive(s".word 6"),
          Directive(s".ascii \"false\\0\"")
        )
    }

    def printBoolLiterFunc(idxTrue: Int, idxFalse: Int): List[Instruction] = {
        List(
          Label("p_print_bool"),
          PushInstr(lr),
          CmpInstr(Reg(0), ImmOffset(0)),
          LoadLabelInstr(Reg(0), s"msg_$idxTrue:", Condition.NE),
          LoadLabelInstr(Reg(0), s"msg_$idxFalse:", Condition.EQ),
          AddInstr(Reg(0), Reg(0), ImmOffset(4)),
          BranchLinkInstr("printf"),
          MoveInstr(Reg(0), ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(pc)
        )
    }

    def printBoolLiter() = {

        /** Add DataMsg for Bool Literal true & false */
        Int idxTrue = collector.tickDataMsg()
        Int idxFalse = collector.tickDataMsg()
        collector.addDataMsg(
          getPrintTrueDirective(idxTrue)
        )
        collector.addDataMsg(
          getPrintFalseDirective(idxFalse)
        )

        /** Add p_print_bool function */
        collector.addFunc(
          printBoolLiterFunc(idxTrue, idxFalse)
        )

        /** Add branch instruction Statement */
        collector.addStatement(
          List(BranchLinkInstr("p_print_bool"))
        )
    }

    /** Print Char Liter */
    def printCharLiter() = {

        /** Branch to putchar */
        collector.addStatement(
          List(
            BranchLinkInstr("putchar"),
            MoveInstr(Reg(0), ImmOffset(0)),
            PopInstr(pc)
          )
        )
    }

    /** Print String Literal */
    def getPrintStrDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx:"),
          Directive(s".word 5"),
          Directive(s".ascii \"%.*s\\0\"")
        )
    }

    def printStrLiterFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_print_string"),
          PushInstr(lr),
          LoadRegMemInstr(Reg(1), Reg(0)),
          AddInstr(Reg(2), Reg(0), ImmOffset(4)),
          LoadLabelInstr(Reg(0), s"msg_$idx:"),
          AddInstr(Reg(0), Reg(0), ImmOffset(4)),
          BranchLinkInstr("printf"),
          MoveInstr(Reg(0), ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(pc)
        )
    }

    /** Enumerations: Condition Codes, Flags */
    object UtilFlag extends Enumeration {
        type UtilFlag = Value
        val PPrintString, PPrintLn, PPrintInt, PPrintRef, PThrowOverflowError,
            PRuntimeError, PDivisionByZeroError, PCheckArrayBounds, PReadChar,
            PReadInt, PFreePair, PCheckNullPointer = Value
    }

    def printStrLiter() = {

        /** Add DataMsg for string formating */
        Int idx = collector.tickDataMsg()
        collector.addDataMsg(getPrintStrDirective(idx))

        /** Add p_print_string function */
        collector.addFunc(
          printStrLiterFunc(idx)
        )

        /** Add branch instruction statement */
        collector.addStatement(
          List(BranchLinkInstr("p_print_string"))
        )
    }

    def printIntLiterFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_print_int"),
          PushInstr(lr),
          MoveInstr(Reg(1), Reg(0)),
          LoadImmLabelInstr(Reg(0), s"msg_$idx:"),
          AddInstr(Reg(0), Reg(0), ImmOffset(4)),
          BranchLinkInstr("printf"),
          MoveInstr(Reg(0), ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(pc)
        )
    }

}