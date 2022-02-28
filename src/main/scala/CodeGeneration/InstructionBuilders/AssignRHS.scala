import Helpers._
import Condition._
import constants._

object transRHS {
    /* Returns a list of instructions evaluating the RHS of an assignment */
    def apply(rhs: AssignRHSNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        rhs match {
            case e: ExprNode => transExpression(e, stackFrame)
            case NewPairNode(e1, e2) => {
                addNewPairElem(e1, stackFrame)
                addNewPairElem(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(Reg(0), ImmOffset(8)),
                    BranchLinkInstr("malloc", AL),
                    PopInstr(List(Reg(1), Reg(2))),
                    StoreInstr(Reg(2), Reg(0), ImmOffset(0), false),
                    StoreInstr(Reg(1), Reg(0), ImmOffset(4), false)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE * 2)
            }
            case CallNode(i, args) => {
                val FunctionId(t, plist, _) = stackFrame.st.lookupAll(i.s).get
                var offset = 0

                /** Push params into stack */
                (plist zip args).reverse.foreach {
                    case (p, e) => {
                        transExpression(e, stackFrame)
                        collector.addStatement(List(p match {
                            case Param(CharType()) | Param(BoolType()) => {
                                offset += BIT_SIZE
                                StoreByteInstr(
                                  r0,
                                  sp,
                                  ImmOffset(-BIT_SIZE),
                                  true
                                )
                            }
                            case _ => {
                                offset += WORD_SIZE
                                StoreInstr(r0, sp, ImmOffset(-WORD_SIZE), true)
                            }
                        }))
                    }
                }

                /** Branch to function and recover stack */
                collector.addStatement(
                  List(
                    BranchLinkInstr(s"f_${i.s}", Condition.AL),
                    AddInstr(sp, sp, ImmOffset(offset))
                  )
                )

            }
            case _ =>
        }
    }
}
