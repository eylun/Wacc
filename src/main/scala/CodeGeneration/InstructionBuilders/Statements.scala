object transStatement {
    def apply(statNode: StatNode)(implicit
        collector: WaccBuffer
    ): List[Instruction] =
        // TODO: Each statNode match should return a list of instructions
        // it should call translation functions on all appropriate parts of
        // the statement, this also means that it should call transStatement
        // on nested statements (like in if-then-else, while-do, begin-end)
        statNode match {
            case _ => List[Instruction]().empty
        }
}
