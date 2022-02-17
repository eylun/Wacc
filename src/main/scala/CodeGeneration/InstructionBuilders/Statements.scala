object transStatement {
    def apply(statNode: StatNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit =
        // TODO: Each statNode match should return a list of instructions
        // it should call translation functions on all appropriate parts of
        // the statement, this also means that it should call transStatement
        // on nested statements (like in if-then-else, while-do, begin-end)
        collector.addStatement(stackFrame.emit(statNode match {
            case ite @ IfThenElseNode(e, s1, s2) => {
                transStatement(
                  s1,
                  stackFrame.join(StackFrame(ite.newScopeST1))
                )
                transStatement(
                  s2,
                  stackFrame.join(StackFrame(ite.newScopeST2))
                )
                List[Instruction]().empty
            }
            case _ => List[Instruction]().empty
        }))
}
