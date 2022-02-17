import constants._

object CodeGenerator {
    // TODO: Call transProg on progNode whenever that is ready
    def apply(progNode: ProgramNode, st: SymbolTable)(implicit
        collector: WaccBuffer
    ): List[Instruction] = {
        val mainStackFrame = StackFrame(st)
        transStatement(progNode.s, mainStackFrame)
        progNode.flist.foreach { transFunction(_) }
        collector.addStatement(
          List(MoveInstr(Reg(0), ImmOffset(0)), PopInstr(pc))
        )
        collector.emit()
    }
}
