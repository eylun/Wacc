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
            case al @ ArrayLiterNode(es) => {
                al.typeId.get match {
                    case AnyType() => {
                        collector.addStatement(
                          List(
                            MoveInstr(
                              Reg(0),
                              ImmOffset(WORD_SIZE)
                            )
                          )
                        )
                    }
                    case ArrayType(t, _, d) => {
                        collector.addStatement(
                          List(
                            MoveInstr(
                              Reg(0),
                              ImmOffset(getArraySize(t, es.length))
                            )
                          )
                        )
                    }
                    case _ =>
                }
                collector.addStatement(
                  List(
                    BranchLinkInstr("malloc", Condition.AL),
                    MoveInstr(Reg(3), RegOp(Reg(0)))
                  )
                )
                var ofs = WORD_SIZE
                es.foreach { e =>
                    {
                        transExpression(e, stackFrame)
                        e.typeId.get.getType() match {
                            case CharType() | BoolType() => {
                                collector.addStatement(
                                  List(
                                    StoreByteInstr(
                                      Reg(0),
                                      Reg(3),
                                      ImmOffset(ofs)
                                    )
                                  )
                                )
                                ofs += BIT_SIZE
                            }
                            case _ => {
                                collector.addStatement(
                                  List(
                                    StoreInstr(Reg(0), Reg(3), ImmOffset(ofs))
                                  )
                                )
                                ofs += WORD_SIZE
                            }
                        }
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
                    AddInstr(sp, sp, ImmOffset(offset), false)
                  )
                )

            }
            case e: PairElemNode => {
                collector.insertUtil(UtilFlag.PCheckNullPointer)
                collector.addStatement(
                  List(
                    BranchLinkInstr("p_check_null_pointer")
                  ) ++
                      (e match {
                          case FirstPairElemNode(f) => {
                              transExpression(f, stackFrame)
                              List(
                                LoadInstr(r0, r0, ImmOffset(0))
                              )
                          }
                          case SecondPairElemNode(s) => {
                              transExpression(s, stackFrame)
                              List(
                                LoadInstr(
                                  r0,
                                  r0,
                                  ImmOffset(WORD_SIZE)
                                )
                              )
                          }
                      }) ++ List(LoadInstr(r0, r0, ImmOffset(0)))
                )
            }
        }
    }
}
