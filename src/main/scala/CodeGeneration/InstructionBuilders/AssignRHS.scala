object transRHS {
    /* Returns a list of instructions evaluating the RHS of an assignment */
    def apply(rhs: AssignRHSNode, stackFrame: StackFrame)
            (implicit collector: WaccBuffer): List[Instruction] = {
        rhs match {
            case e: ExprNode => transExpression(e)
            case _ => List()
        }
    }
}
