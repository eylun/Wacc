import Helpers._
import Helpers.UtilFlag._
import constants._

object transExpression {
    def apply(exprNode: ExprNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit =
        // TODO: Each ExprNode match should return a list of instructions
        exprNode match {
            case IdentNode(s) =>
                stackFrame.st.lookupAll(s).get.getType() match {
                    case CharType() => {
                        collector.addStatement(
                          List(
                            LoadRegSignedByte(
                              Reg(0),
                              sp,
                              ImmOffset(stackFrame.getOffset(s))
                            )
                          )
                        )
                    }
                    case _ => {
                        collector.addStatement(
                          List(
                            // TODO: Check this again in the future when people reply on edstem
                            // For some reason ImmOffset for pairs should have 4 added to it
                            LoadInstr(
                              Reg(0),
                              sp,
                              ImmOffset(stackFrame.getOffset(s))
                            )
                          )
                        )
                    }
                }

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
                // slight duplication on this for loop, see if can merge
                var count = 0
                es.foreach { e =>
                    { 
                        transExpression(e, stackFrame)
                        collector.addStatement(
                            List(
                                BranchLinkInstr("p_check_array_bounds", Condition.AL),
                                AddInstr(Reg(4), Reg(4), ImmOffset(4), false),                                        
                            )
                        )
                        count = count + 1
                        collector.addStatement(
                            if (count == es.length) {
                                    ae.typeId.get.getType() match {
                                        case CharType() | BoolType() => 
                                            List(
                                                AddInstr(Reg(4), Reg(4), RegOp(Reg(0)), false),
                                                LoadRegSignedByte(Reg(4), Reg(4), ImmOffset(0))
                                                )
                                        case _ => 
                                            List(
                                                AddInstr(Reg(4), Reg(4), LSLRegOp(Reg(0), ShiftImm(2)), false),
                                                LoadInstr(Reg(4), Reg(4), ImmOffset(0))
                                                )
                                    }

                            } else {
                                List(
                                    AddInstr(Reg(4), Reg(4), LSLRegOp(Reg(0), ShiftImm(2)), false),
                                    LoadInstr(Reg(4), Reg(4), ImmOffset(0))
                                )
                            }
                        )
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
                // TODO
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
            }
            case Sub(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
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
            }
            case Mult(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
                transExpression(e2, stackFrame)

                collector.insertUtil(PThrowOverflowError)

                collector.addStatement(
                  List(
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SMullInstr(Reg(0), Reg(1), Reg(0), Reg(1)),
                    CompareInstr(Reg(0), ASRRegOp(Reg(0), ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE)
                  )
                )
            }
            case Div(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
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
            }
            case Mod(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
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
            }
            case And(e1, e2) => {
                transExpression(e1, stackFrame)
                // Short-circuit evaluation
                collector.addStatement(
                  List(
                    CompareInstr(Reg(0), ImmOffset(0)),
                    BranchInstr("L0", Condition.EQ)
                  )
                )
                transExpression(e2, stackFrame)

                collector.addStatement(
                  List(
                    Label("L0")
                  )
                )
            }
            case Or(e1, e2) => {
                transExpression(e1, stackFrame)
                // Short-circuit evaluation
                collector.addStatement(
                  List(
                    CompareInstr(Reg(0), ImmOffset(1)),
                    BranchInstr("L0", Condition.EQ)
                  )
                )
                transExpression(e2, stackFrame)

                collector.addStatement(
                  List(
                    Label("L0")
                  )
                )
            }
            case GT(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
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
            }
            case GTE(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
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
            }
            case LT(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
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
            }
            case LTE(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
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
            }
            case Equal(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
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
            }
            case NotEqual(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(Reg(0)))))
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
            }
            case _ => List[Instruction]().empty
        }
}
