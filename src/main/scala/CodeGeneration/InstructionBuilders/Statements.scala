import Condition._
import Helpers._

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
                        t match {
                            case arr_t @ ArrayTypeNode(at, dimension) => {
                                // will always be 4 here, so not sure if necessary
                                val arrayAddrSize = getTypeSize(t.typeId.get)
                                r match {
                                    // TODO: possible to put this in assignRHS instead BUT
                                    // the only way to get size is by getting type id of first element
                                    case ArrayLiterNode(es) => {
                                        collector.addStatement(
                                          List(
                                            MoveInstr(
                                              Reg(0),
                                              ImmOffset(
                                                getArraySize(
                                                  arr_t,
                                                  es.length
                                                ) + arrayAddrSize
                                              )
                                            ),
                                            BranchLinkInstr("malloc"),
                                            MoveInstr(Reg(3), RegOp(Reg(0)))
                                          )
                                        )
                                        val elemSize =
                                            getTypeSize(at.typeId.get)
                                        var ofs = arrayAddrSize
                                        es.foreach { e =>
                                            {
                                                transExpression(e, stackFrame)
                                                collector.addStatement(
                                                  List(
                                                    // set to store byte for char and bool
                                                    StoreInstr(
                                                      Reg(0),
                                                      Reg(3),
                                                      ImmOffset(ofs)
                                                    )
                                                  )
                                                )
                                                ofs = ofs + elemSize
                                            }
                                        }
                                        collector.addStatement(
                                          List(
                                            MoveInstr(
                                              Reg(0),
                                              ImmOffset(es.length)
                                            ),
                                            StoreInstr(
                                              Reg(0),
                                              Reg(3),
                                              ImmOffset(0)
                                            ),
                                            MoveInstr(Reg(3), RegOp(Reg(0))),
                                            StoreInstr(
                                              Reg(0),
                                              StackPtrReg(),
                                              ImmOffset(
                                                stackFrame.getOffset(i.s)
                                              )
                                            )
                                          )
                                        )

                                    }
                                    case _ => transRHS(r, stackFrame)
                                }
                            }
                            case _ => {
                                transRHS(r, stackFrame)
                            }
                        }
                        // all new assign code ends with storing reg 0 into stack
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
                        val labelTrue = s"ite_${collector.tickIte()}"
                        val labelFalse = s"ite_${collector.tickIte()}"
                        collector.addStatement(
                          List(
                            CompareInstr(Reg(0), ImmOffset(0), AL),
                            BranchInstr(labelFalse, EQ)
                          )
                        )
                        transStatement(
                          s1,
                          stackFrame.join(StackFrame(ite.trueST), ite.trueST)
                        )
                        collector.addStatement(
                          List(BranchInstr(labelTrue, AL), Label(labelFalse))
                        )
                        transStatement(
                          s2,
                          stackFrame.join(StackFrame(ite.falseST), ite.falseST)
                        )
                        collector.addStatement(List(Label(labelTrue)))
                    }
                    case be @ BeginEndNode(s) => {
                        transStatement(
                          s,
                          stackFrame.join(
                            StackFrame(be.newScopeST),
                            be.newScopeST
                          )
                        )
                    }
                    case wd @ WhileDoNode(e, s) => {
                        val labelCheck = s"wd_${collector.tickWd()}"
                        val labelContent = s"wd_${collector.tickWd()}"
                        collector.addStatement(
                          List(BranchInstr(labelCheck, AL), Label(labelContent))
                        )
                        transStatement(
                          s,
                          stackFrame.join(
                            StackFrame(wd.newScopeST),
                            wd.newScopeST
                          )
                        )
                        collector.addStatement(List(Label(labelCheck)))
                        transExpression(e, stackFrame)
                        collector.addStatement(
                          (
                            List(
                              CompareInstr(Reg(0), ImmOffset(1), AL),
                              BranchInstr(labelContent, EQ)
                            )
                          )
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
