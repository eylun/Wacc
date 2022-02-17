import Helpers._
import constants._

object transExpression {
    def apply(exprNode: ExprNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit =
        // TODO: Each ExprNode match should return a list of instructions
        exprNode match {
            case IdentNode(s) =>
                collector.addStatement(
                  List(
                    LoadInstr(Reg(0), sp, ImmOffset(stackFrame.getOffset(s)))
                  )
                )
            case IntLiterNode(n) =>
                collector.addStatement(List(LoadImmIntInstr(Reg(0), n)))
            case CharLiterNode(c) =>
                collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(c))))
            case BoolLiterNode(true) =>
                collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(1))))
            case BoolLiterNode(false) =>
                collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(0))))
            case StringLiterNode(str) => {
                val msgCount = collector.tickDataMsg()
                collector.addDataMsg(
                  getStringDirective(str, msgCount)
                )
                collector.addStatement(
                  List(LoadImmLabelInstr(Reg(0), s"msg_$msgCount"))
                )
            }
            case Add(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(Reg(0))))
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(Reg(0)),
                    AddInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    BranchLinkInstr("exit", Condition.AL)
                  )
                )
            } // TODO: create labels for BLs
            case Sub(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(Reg(0))))
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(Reg(0)),
                    SubInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    BranchLinkInstr("exit", Condition.AL)
                  )
                )
            } // TODO: create labels for BLs
            case Mult(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(Reg(0))))
                transExpression(e2, stackFrame)
                collector.addStatement(
                    List(
                        MoveInstr(Reg(1), RegOp(Reg(0))),
                        PopInstr(Reg(0)),
                        SMullInstr(Reg(0), Reg(1), Reg(0), Reg(1)),
                        CmpInstr(Reg(0), RegOp(Reg(1))),
                        BranchLinkInstr("p_throw_overflow_error", Condition.NE),
                        BranchLinkInstr("exit", Condition.AL)
                    )
                )
            } // TODO: handle overflow - CMP r1, r0, ASR #31, create labels for BLs
            case Div(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(Reg(0))))
                transExpression(e2, stackFrame)
                collector.addStatement(
                    List(
                        MoveInstr(Reg(1), RegOp(Reg(0))),
                        PopInstr(Reg(0)),
                        BranchLinkInstr("p_check_divide_by_zero", Condition.AL),
                        BranchLinkInstr("__aeabi_idiv", Condition.AL),
                        BranchLinkInstr("exit", Condition.AL)
                    )
                )
            } // TODO: create labels for the BLs
            case _ =>
        }
}
