import constants._
import Helpers._

/** Adds instructisons for a new function into the wacc buffer */
object transFunction {
    def apply(funcNode: FuncNode, stackFrame: StackFrame)
        (implicit collector: WaccBuffer, repr: Representation): Unit = {
        val FuncNode(_, i, plist, s) = funcNode
        plist.foreach(p => {
            stackFrame.unlock(p.i.s)
        })
        repr match {
            case ARMRepresentation => collector.addStatement(
                List(Label(s"f_${i.s}"), PushInstr(List(lr))) ++ stackFrame.head
            )
            case X86Representation => collector.addStatement(
                List(Label(s"f_${i.s}"), PushInstr(List(lr)), MoveInstr(lr, RegOp(sp))) ++ stackFrame.head
            )
        }
        
        transStatement(s, stackFrame)

        collector.addStatement(
          List(
            Directive("ltorg")
          )
        )
    }
}
