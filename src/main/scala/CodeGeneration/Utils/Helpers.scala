import constants._
import transExpression._

object Helpers {
    val WORD_SIZE = 4
    val BIT_SIZE = 1

    val mainSetup = List(
      Directive("text"),
      Directive("global main")
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
          Label(s"msg_$idx"),
          Directive(s"word ${s.length()}"),
          Directive(s"ascii \"$s\"")
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
            BranchLinkInstr("malloc", Condition.AL),
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
        stackFrame.addTempOffset(WORD_SIZE)
    }

    /** PRINT STATEMENT HELPERS */
    /** Print Int Literal */
    def getPrintIntDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 3"),
          Directive(s"ascii \"%d\\0\"")
        )
    }

    def printIntLiterFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_print_int"),
          PushInstr(List(lr)),
          MoveInstr(Reg(1), RegOp(Reg(0))),
          LoadLabelInstr(Reg(0), s"msg_$idx"),
          AddInstr(Reg(0), Reg(0), ImmOffset(4), false),
          BranchLinkInstr("printf"),
          MoveInstr(Reg(0), ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(List(pc))
        )
    }

    def printIntLiter(implicit collector: WaccBuffer) = {

        /** Add DataMsg for the Int literal */
        val idx: Int = collector.tickDataMsg()
        collector.addDataMsg(getPrintIntDirective(idx))

        /** Add p_print_int function */
        collector.addUtilStatement(
          printIntLiterFunc(idx)
        )

    }

    /** Print Bool Literal */
    def getPrintTrueDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 5"),
          Directive(s"ascii \"true\\0\"")
        )
    }

    def getPrintFalseDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 6"),
          Directive(s"ascii \"false\\0\"")
        )
    }

    def printBoolLiterFunc(idxTrue: Int, idxFalse: Int): List[Instruction] = {
        List(
          Label("p_print_bool"),
          PushInstr(List(lr)),
          CompareInstr(Reg(0), ImmOffset(0), Condition.AL),
          LoadLabelInstr(Reg(0), s"msg_$idxTrue", Condition.NE),
          LoadLabelInstr(Reg(0), s"msg_$idxFalse", Condition.EQ),
          AddInstr(Reg(0), Reg(0), ImmOffset(4), false),
          BranchLinkInstr("printf"),
          MoveInstr(Reg(0), ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(List(pc))
        )
    }

    def printBoolLiter(implicit collector: WaccBuffer) = {

        /** Add DataMsg for Bool Literal true & false */
        val idxTrue: Int = collector.tickDataMsg()
        val idxFalse: Int = collector.tickDataMsg()
        collector.addDataMsg(
          getPrintTrueDirective(idxTrue)
        )
        collector.addDataMsg(
          getPrintFalseDirective(idxFalse)
        )

        /** Add p_print_bool function */
        collector.addUtilStatement(
          printBoolLiterFunc(idxTrue, idxFalse)
        )

    }

    /** Print String Literal */
    def getPrintStrDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 5"),
          Directive(s"ascii \"%.*s\\0\"")
        )
    }

    def printStrLiterFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_print_string"),
          PushInstr(List(lr)),
          LoadInstr(Reg(1), Reg(0), ImmOffset(0)),
          AddInstr(Reg(2), Reg(0), ImmOffset(4), false),
          LoadLabelInstr(Reg(0), s"msg_$idx"),
          AddInstr(Reg(0), Reg(0), ImmOffset(4), false),
          BranchLinkInstr("printf"),
          MoveInstr(Reg(0), ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(List(pc))
        )
    }

    def printStrLiter(implicit collector: WaccBuffer) = {

        /** Add DataMsg for string formating */
        val idx: Int = collector.tickDataMsg()
        collector.addDataMsg(getPrintStrDirective(idx))

        /** Add p_print_string function */
        collector.addUtilStatement(
          printStrLiterFunc(idx)
        )

    }

    def getPrintRefDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 3"),
          Directive(s"ascii \"%p\\0\"")
        )
    }

    def printRefFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_print_reference"),
          PushInstr(List(lr)),
          MoveInstr(Reg(1), RegOp(Reg(0))),
          LoadLabelInstr(Reg(0), s"msg_$idx"),
          AddInstr(Reg(0), Reg(0), ImmOffset(4), false),
          BranchLinkInstr("printf"),
          MoveInstr(Reg(0), ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(List(pc))
        )
    }

    def printRef(implicit collector: WaccBuffer) = {

        /** Add DataMsg for string formating */
        val idx: Int = collector.tickDataMsg()
        collector.addDataMsg(getPrintRefDirective(idx))

        /** Add p_print_string function */
        collector.addUtilStatement(
          printRefFunc(idx)
        )

    }

    /** PRINTLN STATEMENT HELPERS */
    def getPrintNewLineDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 1"),
          Directive(s"ascii \"\\0\"")
        )
    }

    def printNewLineFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_print_ln"),
          PushInstr(List(lr)),
          LoadLabelInstr(Reg(0), s"msg_$idx"),
          AddInstr(Reg(0), Reg(0), ImmOffset(4), false),
          BranchLinkInstr("puts"),
          MoveInstr(Reg(0), ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(List(pc))
        )
    }

    def printNewLine(implicit collector: WaccBuffer) = {

        /** Add DataMsg for println newline escape char */
        val idx: Int = collector.tickDataMsg()
        collector.addDataMsg(getPrintNewLineDirective(idx))

        /** Add p_print_int function */
        collector.addUtilStatement(
          printNewLineFunc(idx)
        )
    }

    /** Enumerations: Condition Codes, Flags */
    object UtilFlag extends Enumeration {
        type UtilFlag = Value
        val PPrintInt, PPrintBool, PPrintChar, PPrintString, PPrintRef,
            PPrintNewLine, PThrowOverflowError, PRuntimeError,
            PDivisionByZeroError, PCheckArrayBounds, PReadChar, PReadInt,
            PFreePair, PCheckNullPointer = Value
    }

}
