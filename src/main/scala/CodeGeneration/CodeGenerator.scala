import constants._

object CodeGenerator {
    def apply(progNode: ProgramNode, st: SymbolTable)
        (implicit collector: WaccBuffer, repr: Representation): List[Instruction] = {

        /** Creates the top level stack frame */
        val mainStackFrame = StackFrame(st)

        /** Call transFunction on every function in the program */
        progNode.flist.foreach(f => {
            val FunctionId(_, _, funcSt) = st.lookup(f.i.s).get
            transFunction(f, StackFrame(funcSt))
        })

        /** Add label for "main" instruction sequence and push link register
          * onto stack
          */
        collector.addStatement(List(Label("main"), PushInstr(List(lr))))

        /** Add instructions to decrement stack pointer */
        collector.addStatement(mainStackFrame.head)

        /** Generate instructions for every statement in the program */
        transStatement(progNode.s, mainStackFrame)

        /** Add instructions to increment the stack pointer */
        collector.addStatement(mainStackFrame.tail)

        collector.addStatement(
          List(MoveInstr(r0, ImmOffset(0)), PopInstr(List(pc)))
        )
        collector.emit()
    }
}
