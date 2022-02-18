import Helpers._

object transRHS {
    /* Returns a list of instructions evaluating the RHS of an assignment */
    def apply(rhs: AssignRHSNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        rhs match {
            case e: ExprNode => transExpression(e, stackFrame)
            case NewPairNode(e1, e2) => {
                addNewPairElem(e1, stackFrame)
                addNewPairElem(e2, stackFrame)
                collector.addStatement(
                    List(
                        MoveInstr(Reg(3), ImmOffset(8)),
                        BranchLinkInstr("malloc"),
                        PopInstr(List(Reg(1),Reg(2))),
                        StoreInstr(Reg(2), Reg(0), ImmOffset(0)),
                        StoreInstr(Reg(1), Reg(0), ImmOffset(4))
                    )
                )
            }
            case _           =>
        }
    }
}
