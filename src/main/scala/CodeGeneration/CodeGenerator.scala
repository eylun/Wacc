object CodeGenerator {
    // TODO: Call transProg on progNode whenever that is ready
    def apply(progNode: ProgramNode, st: SymbolTable)(implicit
        collector: WaccBuffer
    ): List[Instruction] = {
        transStatement(progNode.s)
        progNode.flist.foreach { transFunction(_) }
        collector.emit()
    }
}
