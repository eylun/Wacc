import constants._
import Helpers._

object transFunction {
    def apply(funcNode: FuncNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        val FuncNode(_, i, _, s) = funcNode
        collector.addStatement(
          List(Label(s"f_${i.s}"), PushInstr(List(lr))) ++ stackFrame.head
        )
        transStatement(s, stackFrame)

        collector.addStatement(
          List(
            Directive("ltorg")
          )
        )
    }
}
