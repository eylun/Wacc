import Condition._
import constants._

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
    /** Print IntLiteral */
    def getPrintIntDirective(i: Int, idx: Int): List[Instruction] = {
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
          MoveInstr(Reg(1), Reg(0)),
          LoadImmLabelInstr(Reg(0), s"msg_$idx:"),
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
}
