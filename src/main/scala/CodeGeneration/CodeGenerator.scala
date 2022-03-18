import constants._
import OptimisationFlag._
import PeepholeOptimisation._

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

        /** Add label for "main" instruction sequence and push link register onto stack
          */

        repr match {
            case ARMRepresentation => collector.addStatement(List(Label("main"), PushInstr(List(lr))))
            case X86Representation => collector.addStatement(List(
                    Label("_start"), 
                    PushInstr(List(lr)), 
                    MoveInstr(lr, RegOp(sp))
                ))
        }

        /** Add instructions to decrement stack pointer */
        collector.addStatement(mainStackFrame.head)

        /** Generate instructions for every statement in the program */
        transStatement(progNode.s, mainStackFrame)

        /** Add instructions to increment the stack pointer */
        collector.addStatement(mainStackFrame.tail)

        collector.addStatement(
          List(MoveInstr(r0, ImmOffset(0)), PopInstr(List(pc)))
        )

        /** Execute optimisation function(s) if flag is set */
        collector.optFlag match {
            case OptimisationFlag.O0  => collector.emit()
            case OptimisationFlag.Oph => executePeepholeOptimisation(collector.emit())
        }
    }
}
