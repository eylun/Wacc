import Helpers._
import Helpers.UtilFlag._
import constants._

/** Adds the appropriate instructions for each expression into our Wacc Buffer collector
  */
object transExpression {
    def apply(exprNode: ExprNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer,
        repr: Representation
    ): Unit = {
        exprNode match {
            /** IDENTIFIER */
            case IdentNode(s) => {
                collector.addStatement(
                  List(
                    /** Adds a different load instruction depending on the identifier type
                      */
                    determineLoadInstr(
                      stackFrame.currST.lookupAll(s).get.getType(),
                      r0,
                      sp,
                      stackFrame.getOffset(s)
                    )
                  )
                )
            }

            /** LITERALS: int, char, bool, string, pair, array-elem */
            case IntLiterNode(n) =>
                collector.addStatement(List(LoadImmIntInstr(r0, n)))
            case CharLiterNode(c) =>
                collector.addStatement(List(MoveInstr(r0, ImmOffset(c))))
            case BoolLiterNode(true) =>
                collector.addStatement(List(MoveInstr(r0, ImmOffset(1))))
            case BoolLiterNode(false) =>
                collector.addStatement(List(MoveInstr(r0, ImmOffset(0))))
            case StringLiterNode(str) => {
                val msgCount = collector.tickDataMsg()
                collector.addDataMsg(
                  getStringDirective(str, msgCount)
                )
                collector.addStatement(
                  List(LoadLabelInstr(r0, s"msg_$msgCount"))
                )
            }
            case PairLiterNode() =>
                collector.addStatement(List(MoveInstr(r0, ImmOffset(0))))

            case ae @ ArrayElemNode(IdentNode(s), es) => {
                collector.insertUtil(UtilFlag.PCheckArrayBounds)
                collector.addStatement(
                  List(
                    LoadInstr(r0, sp, ImmOffset(stackFrame.getOffset(s))),
                    PushInstr(List(r4)),
                    MoveInstr(r4, RegOp(r0))
                  )
                )
                stackFrame.addTempOffset(ARRAY_EXP_OFFSET)
                es.zipWithIndex.foreach {
                    case (e, idx) => {
                        transExpression(e, stackFrame)

                        collector.addStatement(
                          List(
                            BranchLinkInstr("p_check_array_bounds", Condition.AL),
                            AddInstr(r4, r4, ImmOffset(WORD_SIZE), false)
                          )
                        )
                        
                        collector.addStatement(ae.typeId.get.getType() match {
                            case CharType() | BoolType() if idx == es.length - 1 =>
                                List(
                                  AddInstr(r4, r4, RegOp(r0), false),
                                  LoadRegSignedByte(r4, r4, ImmOffset(0))
                                )
                            case _ => {
                                List(
                                    AddInstr(r4, r4, LSLRegOp(r0, ShiftImm(TYPE_SHIFT)), false),
                                    LoadInstr(r4, r4, ImmOffset(0))
                                )
                            }
                        })
                    }
                }
                collector.addStatement(
                  List(
                    MoveInstr(r0, RegOp(r4)),
                    PopInstr(List(r4))
                  )
                )
                stackFrame.dropTempOffset(ARRAY_EXP_OFFSET)
            }

            /** UNARY OPERATIONS */
            case Not(e) => {
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(
                    XorInstr(r0, r0, ImmOffset(1))
                  )
                )
            }
            case Neg(e) => {
                transExpression(e, stackFrame)

                collector.insertUtil(PThrowOverflowError)
                
                repr match {
                    case ARMRepresentation => collector.addStatement(
                        List(
                            ReverseSubInstr(r0, r0, ImmOffset(0), true),
                            BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                        )
                    )
                    case X86Representation => collector.addStatement(
                        List(
                            MoveInstr(r1, RegOp(r0)),
                            ReverseSubInstr(r0, r1, ImmOffset(0), true),
                            BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                        )
                    )
                }
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

            /* * BINARY OPERATIONS */
            case Add(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)

                collector.insertUtil(PThrowOverflowError)

                collector.addStatement(
                  List(
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    AddInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case Sub(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)

                collector.insertUtil(PThrowOverflowError)

                collector.addStatement(
                  List(
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SubInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case Mult(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)

                collector.insertUtil(PThrowOverflowError)

                collector.addStatement(
                  List(
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SMullInstr(r0, r1, r0, r1),
                    CompareInstr(r1, ASRRegOp(r0, ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case Div(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)

                collector.insertUtil(PCheckDivideByZero)

                repr match {
                    case ARMRepresentation => collector.addStatement(
                        List(
                            MoveInstr(r1, RegOp(r0)),
                            PopInstr(List(r0)),
                            BranchLinkInstr("p_check_divide_by_zero"),
                            BranchLinkInstr("__aeabi_idiv")
                        )
                    )
                    case X86Representation => collector.addStatement(
                        List(
                            MoveInstr(r1, RegOp(r0)),
                            PopInstr(List(r0)),
                            BranchLinkInstr("p_check_divide_by_zero"),
                            XorInstr(r2, r2, RegOp(r2)),
                            SDivInstr(r1)
                        )
                    )
                }
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case Mod(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)

                collector.insertUtil(PCheckDivideByZero)

                repr match {
                    case ARMRepresentation => collector.addStatement(
                        List(
                            MoveInstr(r1, RegOp(r0)),
                            PopInstr(List(r0)),
                            BranchLinkInstr("p_check_divide_by_zero"),
                            BranchLinkInstr("__aeabi_idivmod"),
                            MoveInstr(r0, RegOp(r1))
                        )
                    )
                    case X86Representation => collector.addStatement(
                        List(
                            MoveInstr(r1, RegOp(r0)),
                            PopInstr(List(r0)),
                            BranchLinkInstr("p_check_divide_by_zero"),
                            XorInstr(r2, r2, RegOp(r2)),
                            SDivInstr(r1),
                            MoveInstr(r0, RegOp(r2))
                        )
                    )
                }
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case And(e1, e2) => {
                transExpression(e1, stackFrame)
                val label = collector.tickGeneral()

                /** short-circuit evaluation */
                collector.addStatement(
                  List(
                    CompareInstr(r0, ImmOffset(0)),
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
                transExpression(e1, stackFrame)
                val label = collector.tickGeneral()

                /** short-circuit evaluation */
                collector.addStatement(
                  List(
                    CompareInstr(r0, ImmOffset(1)),
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
            /** Greater-Than operator */
            case GT(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.GT),
                    MoveInstr(r0, ImmOffset(0), Condition.LE)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            /** Greater-Than-Or-Equals operator */
            case GTE(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.GE),
                    MoveInstr(r0, ImmOffset(0), Condition.LT)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            /** Less-Than operator */
            case LT(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.LT),
                    MoveInstr(r0, ImmOffset(0), Condition.GE)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            /** Less-Than-Or-Equals operator */
            case LTE(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.LE),
                    MoveInstr(r0, ImmOffset(0), Condition.GT)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case Equal(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.EQ),
                    MoveInstr(r0, ImmOffset(0), Condition.NE)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case NotEqual(e1, e2) => {
                transExpression(e1, stackFrame)
                collector.addStatement(List(PushInstr(List(r0))))
                stackFrame.addTempOffset(WORD_SIZE)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.NE),
                    MoveInstr(r0, ImmOffset(0), Condition.EQ)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE)
            }
            case _ => List[Instruction]().empty
        }
    }
}
