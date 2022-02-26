object transFunction {
    def apply(funcNode: FuncNode)(implicit
        collector: WaccBuffer
    ): Unit =
        // TODO: Each ExprNode match should return a list of instructions
        funcNode match {
            case _ =>
        }
}
