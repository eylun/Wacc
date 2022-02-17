object transStatement {
    def apply(statList: StatNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        // TODO: Each statNode match should return a list of instructions
        // it should call translation functions on all appropriate parts of
        // the statement, this also means that it should call transStatement
        // on nested statements (like in if-then-else, while-do, begin-end)
        collector.addStatement(stackFrame.head)
        statList match {
            case StatListNode(l) => {
                l.foreach {
                    case NewAssignNode(t, i, r) => {
                        transRHS(r, stackFrame)
                        collector.addStatement(
                          List(
                            StoreInstr(
                              Reg(0),
                              StackPtrReg(),
                              ImmOffset(stackFrame.getOffset(i.s))
                            )
                          )
                        )
                    }
                    case LRAssignNode(l, r) => {
                        l match {
                            case IdentNode(i) => {
                                transRHS(r, stackFrame)
                                collector.addStatement(
                                  List(
                                    StoreInstr(
                                      Reg(0),
                                      StackPtrReg(),
                                      ImmOffset(stackFrame.getOffset(i))
                                    )
                                  )
                                )
                            }
                            case _ => // TODO complete for array and pair elems
                        }
                    }
                    case ite @ IfThenElseNode(e, s1, s2) => {
                        transStatement(
                          s1,
                          stackFrame.join(StackFrame(ite.newScopeST1))
                        )
                        transStatement(
                          s2,
                          stackFrame.join(StackFrame(ite.newScopeST2))
                        )
                    }
                    case SkipNode() =>
                    case ExitNode(e) => {
                        transExpression(e, stackFrame)
                        collector.addStatement(
                          List(
                            BranchLinkInstr("exit")
                          )
                        )
                    }
                    case _ =>
                }
            }
            /** The match will never reach here as only statlists are passed
              * into transStat
              */
            case _ =>
        }
        collector.addStatement(stackFrame.tail)
    }
}
