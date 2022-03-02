import constants._

object CodeGenerator {
    def apply(progNode: ProgramNode, st: SymbolTable)(implicit
        collector: WaccBuffer
    ): List[Instruction] = {
        val mainStackFrame = StackFrame(st)
        progNode.flist.foreach(f => {
            val FunctionId(_, _, funcSt) = st.lookup(f.i.s).get
            transFunction(f, StackFrame(funcSt))
        })
        collector.addStatement(List(Label("main"), PushInstr(List(lr))))

        collector.addStatement(mainStackFrame.head)
        transStatement(progNode.s, mainStackFrame)
        collector.addStatement(mainStackFrame.tail)

        collector.addStatement(
          List(MoveInstr(Reg(0), ImmOffset(0)), PopInstr(List(pc)))
        )
        collector.emit()
    }
}
