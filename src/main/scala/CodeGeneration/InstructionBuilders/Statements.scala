import Condition._
import Helpers._
import Condition._

object transStatement {
    def apply(statList: StatNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        // TODO: Each statNode match should return a list of instructions
        // it should call translation functions on all appropriate parts of
        // the statement, this also means that it should call transStatement
        // on nested statements (like in if-then-else, while-do, begin-end)
        collector.addStatement(stackFrame.head)
        val StatListNode(l) = statList
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
                                    BranchLinkInstr("malloc", AL),
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
                                    MoveInstr(
                                      Reg(0),
                                      RegOp(
                                        Reg(3)
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
                    case ArrayElemNode(i, es) =>
                    // TODO complete for array and pair elems
                    case FirstPairElemNode(e)  => // TODO complete for first
                    case SecondPairElemNode(e) => // TODO complete for second
                }
            }
<<<<<<< HEAD
            case ite @ IfThenElseNode(e, s1, s2) => {
                val labelFalse = s"ite_${collector.tickIte()}"
                val labelTrue = s"ite_${collector.tickIte()}"
                transExpression(e, stackFrame)
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
                val labelContent = s"wd_${collector.tickWd()}"
                val labelCheck = s"wd_${collector.tickWd()}"
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
=======
            case SkipNode() => { List[Instruction]() }
>>>>>>> feat: print statement case for pair literal
            case ExitNode(e) => {
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(
                    BranchLinkInstr("exit", AL)
                  )
                )
            }
            case PrintNode(e) => {
                e match {
                    case IntLiterNode(_) => {

                        /** Add DataMsg for the Int literal */
                        Int idx = collector.tickDataMsg()
                        collector.addDataMsg(getPrintIntDirective(i, idx))

                        /** Add p_print_int function */
                        collector.addFunc(
                          printIntLiterFunc(idx)
                        )

                        /** Call transExpression and branch */
                        transExpression(e) ++
                            List(BranchLinkInstr("p_print_int"))
                    }

                    case BoolLiterNode(_) => {

                        /** Add DataMsg for Bool Literal true & false */
                        Int idxTrue = collector.tickDataMsg()
                        Int idxFalse = collector.tickDataMsg()
                        collector.addDataMsg(
                          getPrintTrueDirective(idxTrue)
                        )
                        collector.addDataMsg(
                          getPrintFalseDirective(idxFalse)
                        )

                        /** Add p_print_bool function */
                        collector.addFunc(
                          printBoolLiterFunc(idxTrue, idxFalse)
                        )

                        /** Call transExpression and branch */
                        transExpression(e) ++
                            List(BranchLinkInstr("p_print_bool"))
                    }

                    case CharLiterNode(_) => {
                        transExpression(e) ++
                            List(
                              BranchLinkInstr("putchar"),
                              MoveInstr(Reg(0), ImmOffset(0)),
                              PopInstr(pc)
                            )
                    }

                    case StrLiterNode(_) || PairLiterNode() => {

                        /** Add DataMsg for string formating */
                        Int idx = collector.tickDataMsg()
                        collector.addDataMsg(getPrintStrDirective(idx))

                        /** Add p_print_string function */
                        collector.addFunc(
                          printStrLiterFunc(idx)
                        )

                        /** Append transedExpr and branch */
                        transExpression(e) ++
                            List(BranchLinkInstr("p_print_string"))
                    }
                }
            }
            case _ =>
        }

        collector.addStatement(stackFrame.tail)
    }
}
