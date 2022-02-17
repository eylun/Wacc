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
                    AddInstr(Reg(0), Reg(0), RegOp(Reg(1)))
                  )
                )
            }
            case _ =>
        }
}
