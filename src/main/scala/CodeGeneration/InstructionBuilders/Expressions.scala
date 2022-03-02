import Helpers._
import Helpers.UtilFlag._
import constants._

object transExpression {
    def apply(exprNode: ExprNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit =
        exprNode match {
            case IdentNode(s) =>
                collector.addStatement(
                  List(
                    determineLoadInstr(
                      stackFrame.st.lookupAll(s).get.getType(),
                      r0,
                      sp,
                      stackFrame.getOffset(s)
                    )
                  )
                )

            /* Literals */
            case IntLiterNode(n) =>
                collector.addStatement(List(LoadImmIntInstr(Reg(0), n)))
            case CharLiterNode(c) =>
                collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(c))))
            case BoolLiterNode(true) =>
                collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(1))))
            case BoolLiterNode(false) =>
                collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(0))))
            case StringLiterNode(str) => {
                val msgCount = collector.tickDataMsg()
                collector.addDataMsg(
                  getStringDirective(str, msgCount)
                )
                collector.addStatement(
                  List(LoadLabelInstr(Reg(0), s"msg_$msgCount"))
                )
            }
            case PairLiterNode() =>
                collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(0))))

            case ae @ ArrayElemNode(IdentNode(s), es) => {
                collector.insertUtil(UtilFlag.PCheckArrayBounds)
                collector.addStatement(
                  List(
                    LoadInstr(Reg(0), sp, ImmOffset(stackFrame.getOffset(s))),
                    PushInstr(List(Reg(4))),
                    MoveInstr(Reg(4), RegOp(Reg(0)))
                  )
                )
                stackFrame.addTempOffset(ARRAY_EXP_OFFSET)
                es.zipWithIndex.foreach {
                    case (e, idx) => {
                        transExpression(e, stackFrame)
                        collector.addStatement(
                          List(
                            BranchLinkInstr(
                              "p_check_array_bounds",
                              Condition.AL
                            ),
                            AddInstr(Reg(4), Reg(4), ImmOffset(4), false)
                          )
                        )
                        collector.addStatement(ae.typeId.get.getType() match {
                            case CharType() | BoolType()
                                if idx == es.length - 1 =>
                                List(
                                  AddInstr(
                                    Reg(4),
                                    Reg(4),
                                    RegOp(Reg(0)),
                                    false
                                  ),
                                  LoadRegSignedByte(
                                    Reg(4),
                                    Reg(4),
                                    ImmOffset(0)
                                  )
                                )
                            case _ =>
                                List(
                                  AddInstr(
                                    Reg(4),
                                    Reg(4),
                                    LSLRegOp(Reg(0), ShiftImm(2)),
                                    false
                                  ),
                                  LoadInstr(Reg(4), Reg(4), ImmOffset(0))
                                )
                        })
                    }
                }
                collector.addStatement(
                  List(
                    MoveInstr(Reg(0), RegOp(Reg(4))),
                    PopInstr(List(Reg(4)))
                  )
                )
                stackFrame.dropTempOffset(ARRAY_EXP_OFFSET)
            }

            /* Unary operations */
            case Not(e) => {
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(
                    XorInstr(Reg(0), Reg(0), ImmOffset(1))
                  )
                )
            }
            case Neg(e) => {
                transExpression(e, stackFrame)

                collector.insertUtil(PThrowOverflowError)

                collector.addStatement(
                  List(
                    ReverseSubInstr(Reg(0), Reg(0), ImmOffset(0), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                  )
                )
            }
            case Len(e) => {
                transExpression(e, stackFrame)
                collector.addStatement(List(LoadInstr(r0, r0, ImmOffset(0))))
            }
            case Ord(e) => {
                transExpression(e, stackFrame)
            }
            case Chr(e) => {
                transExpression(e, stackFrame)
            }

            /* Binary operations */
            case Add(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)

                collector.insertUtil(PThrowOverflowError)

                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    AddInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case Sub(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)

                collector.insertUtil(PThrowOverflowError)

                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SubInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case Mult(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)

                collector.insertUtil(PThrowOverflowError)

                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SMullInstr(Reg(0), Reg(1), Reg(0), Reg(1)),
                    CompareInstr(Reg(1), ASRRegOp(Reg(0), ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case Div(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)

                collector.insertUtil(PCheckDivideByZero)

                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idiv")
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case Mod(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)

                collector.insertUtil(PCheckDivideByZero)

                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idivmod"),
                    MoveInstr(Reg(0), RegOp(Reg(1)))
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case And(e1, e2) => {
                val label = collector.tickGeneral()
                transExpression(e1, stackFrame)
                // Short-circuit evaluation
                collector.addStatement(
                  List(
                    CompareInstr(Reg(0), ImmOffset(0)),
                    BranchInstr(s"L$label", Condition.EQ)
                  )
                )
                transExpression(e2, stackFrame)

                collector.addStatement(
                  List(
                    Label(s"L$label")
                  )
                )
            }
            case Or(e1, e2) => {
                val label = collector.tickGeneral()
                transExpression(e1, stackFrame)
                // Short-circuit evaluation
                collector.addStatement(
                  List(
                    CompareInstr(Reg(0), ImmOffset(1)),
                    BranchInstr(s"L$label", Condition.EQ)
                  )
                )
                transExpression(e2, stackFrame)

                collector.addStatement(
                  List(
                    Label(s"L$label")
                  )
                )
            }
            case GT(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    CompareInstr(Reg(0), RegOp(Reg(1))),
                    MoveInstr(Reg(0), ImmOffset(1), Condition.GT),
                    MoveInstr(Reg(0), ImmOffset(0), Condition.LE)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case GTE(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    CompareInstr(Reg(0), RegOp(Reg(1))),
                    MoveInstr(Reg(0), ImmOffset(1), Condition.GE),
                    MoveInstr(Reg(0), ImmOffset(0), Condition.LT)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case LT(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    CompareInstr(Reg(0), RegOp(Reg(1))),
                    MoveInstr(Reg(0), ImmOffset(1), Condition.LT),
                    MoveInstr(Reg(0), ImmOffset(0), Condition.GE)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case LTE(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    CompareInstr(Reg(0), RegOp(Reg(1))),
                    MoveInstr(Reg(0), ImmOffset(1), Condition.LE),
                    MoveInstr(Reg(0), ImmOffset(0), Condition.GT)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case Equal(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    CompareInstr(Reg(0), RegOp(Reg(1))),
                    MoveInstr(Reg(0), ImmOffset(1), Condition.EQ),
                    MoveInstr(Reg(0), ImmOffset(0), Condition.NE)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case NotEqual(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    CompareInstr(Reg(0), RegOp(Reg(1))),
                    MoveInstr(Reg(0), ImmOffset(1), Condition.NE),
                    MoveInstr(Reg(0), ImmOffset(0), Condition.EQ)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case _ => List[Instruction]().empty
        }
}
