import constants._
import transExpression._

object Helpers {
    val WORD_SIZE = 4
    val BIT_SIZE = 1
    val ARRAY_LHS_OFFSET = 8
    val ARRAY_EXP_OFFSET = 4

    val mainSetup = List(
      Directive("text"),
      Directive("global main")
    )

    def getTypeSize(t: Identifier): Int = {
        t match {
            case BoolType() | CharType() => BIT_SIZE
            case Variable(t)             => getTypeSize(t)
            case Param(t)                => getTypeSize(t)
            /** Functions are not applicable for stack frame sizes */
            case FunctionId(_, _, _) => 0
            case _                   => WORD_SIZE
        }
    }

    def getStringDirective(s: String, idx: Int): List[Instruction] = {

        /** Convert escape characters into printable escape characters */
        val string = escapeConvert(s)
        List(
          Label(s"msg_$idx"),
          Directive(s"word ${s.length()}"),
          Directive(s"ascii \"$string\"")
        )
    }

    def escapeConvert(str: String): String = {
        val sb = new StringBuilder
        str.foreach { c =>
            sb ++= (c match {
                case '\u0000' => "\\u0000"
                case '\b'     => "\\b"
                case '\t'     => "\\t"
                case '\n'     => "\\n"
                case '\f'     => "\\f"
                case '\r'     => "\\r"
                case '\"'     => "\\\""
                case '\''     => "\\'"
                case '\\'     => "\\\\"
                case _        => s"$c"
            })
        }
        sb.toString()
    }

    //TODO: only take ArrayTypeNode
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

    /** Print Reference */
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

    /** Print Newline */
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

        /** Add p_print_ln function */
        collector.addUtilStatement(
          printNewLineFunc(idx)
        )
    }

    /** Print Throw Overflow */
    def getPrintOverflowErrorDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 83"),
          Directive(
            s"ascii \"OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n\\0"
          )
        )
    }
    def printOverflowErrorFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_throw_overflow_error"),
          LoadLabelInstr(r0, s"msg_$idx"),
          BranchLinkInstr("p_throw_runtime_error")
        )
    }
    def printOverflowError(implicit collector: WaccBuffer) = {

        /** Add DataMsg for print overflow error */
        val idx: Int = collector.tickDataMsg()
        collector.addDataMsg(getPrintOverflowErrorDirective(idx))

        /** Add p_throw_overflow_error function */
        collector.addUtilStatement(
          printOverflowErrorFunc(idx)
        )

        /** Add p_throw_runtime_error to the asm file */
        collector.insertUtil(UtilFlag.PRuntimeError)
    }

    /** Print Runtime Error */
    def printRuntimeErrorFunc(): List[Instruction] = {
        List(
          Label("p_throw_runtime_error"),
          BranchLinkInstr("p_print_string"),
          MoveInstr(r0, ImmOffset(-1)),
          BranchLinkInstr("exit")
        )
    }
    def printRuntimeError(implicit collector: WaccBuffer) = {

        /** Add p_throw_runtime_error function */
        collector.addUtilStatement(printRuntimeErrorFunc())

        /** Add p_print_string to the asm file */
        collector.insertUtil(UtilFlag.PPrintString)
    }

    /** Print Check Divide By Zero */
    def printCheckDivideByZeroDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 45"),
          Directive(
            s"ascii \"DivideByZeroError: divide or modulo by zero\\n\\0"
          )
        )
    }
    def printCheckDivideByZeroFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_check_divide_by_zero"),
          PushInstr(List(lr)),
          CompareInstr(r1, ImmOffset(0)),
          LoadLabelInstr(r0, s"msg_$idx", Condition.EQ),
          BranchLinkInstr("p_throw_runtime_error", Condition.EQ),
          PopInstr(List(pc))
        )
    }
    def printCheckDivideByZero(implicit collector: WaccBuffer) = {

        /** Add DataMsg for check divide by zero */
        val idx: Int = collector.tickDataMsg()
        collector.addDataMsg(printCheckDivideByZeroDirective(idx))

        /** Add p_check_divide_by_zero error function */
        collector.addUtilStatement(
          printCheckDivideByZeroFunc(idx)
        )

        /** Add p_throw_runtime_error to the asm file */
        collector.insertUtil(UtilFlag.PRuntimeError)
    }

    /** Print Check Array Bounds */
    def printCheckArrayBoundsDirective(
        largeIdx: Int,
        negIdx: Int
    ): List[Instruction] = {
        List(
          /** Negative Index Data Message */
          Label(s"msg_$negIdx"),
          Directive(s"word 44"),
          Directive(
            s"ascii \"ArrayIndexOutOfBoundsError: negative index\\n\\0"
          ),
          /** Index Too Large Data Message */
          Label(s"msg_$largeIdx"),
          Directive(s"word 45"),
          Directive(
            s"ascii \"ArrayIndexOutOfBoundsError: index too large\\n\\0"
          )
        )
    }
    def printCheckArrayBoundsFunc(
        largeIdx: Int,
        negIdx: Int
    ): List[Instruction] = {
        List(
          Label("p_check_array_bounds"),
          PushInstr(List(lr)),
          CompareInstr(r0, ImmOffset(0)),
          LoadLabelInstr(r0, s"msg_$negIdx", Condition.LT),
          BranchLinkInstr("p_throw_runtime_error", Condition.LT),
          LoadInstr(r1, r4, ImmOffset(0)),
          CompareInstr(r0, RegOp(r1)),
          LoadLabelInstr(r0, s"msg_$largeIdx", Condition.CS),
          BranchLinkInstr("p_throw_runetime_error", Condition.CS),
          PopInstr(List(pc))
        )
    }
    def printCheckArrayBounds(implicit collector: WaccBuffer) = {

        /** Add DataMsg for index too large and negative index errors */
        val largeIdx: Int = collector.tickDataMsg()
        val negIdx: Int = collector.tickDataMsg()
        collector.addDataMsg(printCheckArrayBoundsDirective(largeIdx, negIdx))

        /** Add p_check_array_bounds function */
        collector.addUtilStatement(printCheckArrayBoundsFunc(largeIdx, negIdx))

        /** Add p_throw_runtime_error to the asm file */
        collector.insertUtil(UtilFlag.PRuntimeError)
    }

    /** Print Read Int */
    def printReadIntDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 3"),
          Directive(s"ascii \"%d\\0\"")
        )
    }
    def printReadIntFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_read_int"),
          PushInstr(List(lr)),
          MoveInstr(r1, RegOp(r0)),
          LoadLabelInstr(r0, s"msg_$idx"),
          AddInstr(r0, r0, ImmOffset(4), false),
          BranchLinkInstr("scanf"),
          PopInstr(List(pc))
        )
    }
    def printReadInt(implicit collector: WaccBuffer) = {

        /** Add DataMsg for ReadInt Directive */
        val idx: Int = collector.tickDataMsg()
        collector.addDataMsg(printReadIntDirective(idx))

        /** Add p_read_int function */
        collector.addUtilStatement(printReadIntFunc(idx))
    }

    /** Print Read Char */
    def printReadCharDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 4"),
          Directive(s"ascii \"%c\\0\"")
        )
    }
    def printReadCharFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_read_char"),
          PushInstr(List(lr)),
          MoveInstr(r1, RegOp(r0)),
          LoadLabelInstr(r0, s"msg_$idx"),
          AddInstr(r0, r0, ImmOffset(4), false),
          BranchLinkInstr("scanf"),
          PopInstr(List(pc))
        )
    }
    def printReadChar(implicit collector: WaccBuffer) = {

        /** Add DataMsg for ReadChar Directive */
        val idx: Int = collector.tickDataMsg()
        collector.addDataMsg(printReadCharDirective(idx))

        /** Add p_read_char function */
        collector.addUtilStatement(printReadCharFunc(idx))
    }

    /** Print Free Pair */
    def printFreePairDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 50"),
          Directive(
            s"ascii \"NullReferenceError: dereference a null reference\\n\\0\""
          )
        )
    }
    def printFreePairFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_free_pair"),
          PushInstr(List(lr)),
          CompareInstr(r0, ImmOffset(0)),
          LoadLabelInstr(r0, s"msg_$idx", Condition.EQ),
          BranchInstr("p_throw_runtime_error", Condition.EQ),
          PushInstr(List(r0)),
          LoadInstr(r0, r0, ImmOffset(0)),
          BranchLinkInstr("free"),
          LoadInstr(r0, sp, ImmOffset(0)),
          LoadInstr(r0, r0, ImmOffset(WORD_SIZE)),
          BranchLinkInstr("free"),
          PopInstr(List(r0)),
          BranchLinkInstr("free"),
          PopInstr(List(pc))
        )
    }
    def printFreePair(implicit collector: WaccBuffer) = {

        /** Add DataMsg for FreePair Directive */
        val idx: Int = collector.tickDataMsg()
        collector.addDataMsg(printFreePairDirective(idx))

        /** Add p_free_pair function */
        collector.addUtilStatement(printFreePairFunc(idx))

        /** Add p_throw_runtime_error to the asm file */
        collector.insertUtil(UtilFlag.PRuntimeError)
    }

    /** Print Check Null Pointer */
    def printCheckNullPointerDirective(idx: Int): List[Instruction] = {
        List(
          Label(s"msg_$idx"),
          Directive(s"word 50"),
          Directive(
            s"ascii \"NullReferenceError: dereference a null reference\\n\\0\""
          )
        )
    }
    def printCheckNullPointerFunc(idx: Int): List[Instruction] = {
        List(
          Label("p_check_null_pointer"),
          PushInstr(List(lr)),
          CompareInstr(r0, ImmOffset(0)),
          LoadLabelInstr(r0, s"msg_$idx", Condition.EQ),
          BranchLinkInstr("p_throw_runtime_error", Condition.EQ),
          PopInstr(List(pc))
        )
    }
    def printCheckNullPointer(implicit collector: WaccBuffer) = {

        /** Add DataMsg for CheckNullPointer Directive */
        val idx: Int = collector.tickDataMsg()
        collector.addDataMsg(printCheckNullPointerDirective(idx))

        /** Add p_check_null_pointer */
        collector.addUtilStatement(printCheckNullPointerFunc(idx))

        /** Add p_throw_runtime_error to the asm file */
        collector.insertUtil(UtilFlag.PRuntimeError)
    }

    /** Enumerations: Condition Codes, Flags */
    object UtilFlag extends Enumeration {
        type UtilFlag = Value
        val PPrintInt, PPrintBool, PPrintChar, PPrintString, PPrintRef,
            PPrintNewLine, PThrowOverflowError, PRuntimeError,
            PCheckDivideByZero, PCheckArrayBounds, PReadChar, PReadInt,
            PFreePair, PCheckNullPointer = Value
    }

    def cleanFilename(fn: String): String = fn.take(fn.lastIndexOf("."))
}
