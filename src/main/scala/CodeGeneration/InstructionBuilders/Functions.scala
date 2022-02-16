object transFunction {
    def apply(funcNode: FuncNode)(implicit
        collector: WaccBuffer
    ): List[Instruction] =
        // TODO: Each ExprNode match should return a list of instructions
        funcNode match {
            case _ => List[Instruction]().empty
        }
}
