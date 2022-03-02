import Helpers._
import Condition._
import constants._

object transLHS {
    /* Returns a list of instructions evaluating the RHS of an assignment */
    def apply(lhs: AssignLHSNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        lhs match {
            case IdentNode(s) =>
            case ae @ ArrayElemNode(IdentNode(s), es) => {
                collector.insertUtil(UtilFlag.PCheckArrayBounds)
                stackFrame.addTempOffset(ARRAY_LHS_OFFSET)
                collector.addStatement(
                  List(
                    PushInstr(List(Reg(0), Reg(4))),
                    LoadInstr(
                      Reg(4),
                      sp,
                      ImmOffset(stackFrame.getOffset(s))
                    )
                  )
                )
                //consider using zipWithIndex
                es.zipWithIndex.foreach {
                    case (e, idx) => {
                        transExpression(e, stackFrame)
                        collector.addStatement(
                          List(
                            BranchLinkInstr(
                              "p_check_array_bounds",
                              Condition.AL
                            ),
                            AddInstr(
                              Reg(4),
                              Reg(4),
                              ImmOffset(4),
                              false
                            )
                          )
                        )
                        collector.addStatement(
                          if (idx == es.length - 1) {
                              List(
                                ae.typeId.get.getType() match {
                                    case CharType() | BoolType() =>
                                        AddInstr(
                                          Reg(4),
                                          Reg(4),
                                          RegOp(Reg(0)),
                                          false
                                        )
                                    case _ =>
                                        AddInstr(
                                          Reg(4),
                                          Reg(4),
                                          LSLRegOp(Reg(0), ShiftImm(2)),
                                          false
                                        )
                                }
                              )
                          } else {
                              List(
                                AddInstr(
                                  Reg(4),
                                  Reg(4),
                                  LSLRegOp(Reg(0), ShiftImm(2)),
                                  false
                                ),
                                LoadInstr(Reg(4), Reg(4), ImmOffset(0))
                              )
                          }
                        )
                    }
                }
                stackFrame.dropTempOffset(ARRAY_LHS_OFFSET)
                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(4))),
                    PopInstr(List(Reg(0), Reg(4)))
                  )
                )
            }
            case pe: PairElemNode => {
                collector.insertUtil(UtilFlag.PCheckNullPointer)
                stackFrame.addTempOffset(WORD_SIZE)
                collector.addStatement(
                  List(
                    PushInstr(List(r0))
                  )
                )
                pe match {
                    case FirstPairElemNode(e) => {
                        transExpression(e, stackFrame)
                        collector.addStatement(
                          List(
                            BranchLinkInstr("p_check_null_pointer"),
                            AddInstr(r0, r0, ImmOffset(0), false)
                          )
                        )
                    }
                    case SecondPairElemNode(e) => {
                        transExpression(e, stackFrame)
                        collector.addStatement(
                          List(
                            BranchLinkInstr("p_check_null_pointer"),
                            AddInstr(
                              r0,
                              r0,
                              ImmOffset(WORD_SIZE),
                              false
                            )
                          )
                        )
                    }
                }
                stackFrame.dropTempOffset(WORD_SIZE)
                collector.addStatement(
                  List(
                    PushInstr(List(r0)),
                    LoadInstr(r0, r0, ImmOffset(0)),
                    BranchLinkInstr("free"),
                    MoveInstr(
                      r0,
                      ImmOffset(getTypeSize(pe.typeId.get.getType()))
                    ),
                    BranchLinkInstr("malloc"),
                    PopInstr(List(r1)),
                    StoreInstr(r0, r1, ImmOffset(0)),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0))
                  )
                )
            }
        }
    }
}
