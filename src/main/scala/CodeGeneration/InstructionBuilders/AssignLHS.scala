import Helpers._
import Condition._
import constants._

object transLHS {

    /** Adds a list of instructions evaluating the LHS of an assignment to the Wacc Buffer collector
      */
    def apply(lhs: AssignLHSNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer,
        repr: Representation
    ): Unit = {
        lhs match {
            /** IDENT NODE */
            case IdentNode(s) =>
            /** ARRAY ELEM NODE */
            case ae @ ArrayElemNode(IdentNode(s), es) => {
                collector.insertUtil(UtilFlag.PCheckArrayBounds)
                stackFrame.addTempOffset(ARRAY_LHS_OFFSET)
                collector.addStatement(
                  List(
                    PushInstr(List(r0, r4)),
                    LoadInstr(
                      r4,
                      sp,
                      ImmOffset(stackFrame.getOffset(s))
                    )
                  )
                )

                es.zipWithIndex.foreach {
                    case (e, idx) => {
                        transExpression(e, stackFrame)

                        /** Branch to check_array_bounds */
                        collector.addStatement(
                          List(
                            BranchLinkInstr(
                              "p_check_array_bounds",
                              Condition.AL
                            ),
                            AddInstr(
                              r4,
                              r4,
                              ImmOffset(WORD_SIZE),
                              false
                            )
                          )
                        )

                        /** Include a different add instruction depending on array type
                          */
                        collector.addStatement(
                          if (idx == es.length - 1) {
                              List(
                                ae.typeId.get.getType() match {
                                    case CharType() | BoolType() =>
                                        AddInstr(r4, r4, RegOp(r0), false)
                                    case _ => AddInstr(r4, r4, LSLRegOp(r0, ShiftImm(TYPE_SHIFT)), false)
                                }
                              )
                          } else {
                              List(
                                AddInstr(r4, r4, LSLRegOp(r0, ShiftImm(TYPE_SHIFT)), false),
                                LoadInstr(r4, r4, ImmOffset(0))
                              )
                          }
                        )
                    }
                }
                stackFrame.dropTempOffset(ARRAY_LHS_OFFSET)
                collector.addStatement(
                  List(
                    MoveInstr(r1, RegOp(r4)),
                    PopInstr(List(r0, r4))
                  )
                )
            }
            /** PAIR ELEM NODE */
            case pe: PairElemNode => {
                collector.insertUtil(UtilFlag.PCheckNullPointer)
                stackFrame.addTempOffset(WORD_SIZE)
                collector.addStatement(
                  List(
                    PushInstr(List(r0))
                  )
                )
                pe match {
                    /** Add Instructions take different offsets for first pair element and second pair element
                      */
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
                            AddInstr(r0, r0, ImmOffset(WORD_SIZE), false)
                          )
                        )
                    }
                }
                stackFrame.dropTempOffset(WORD_SIZE)
                repr match {
                    case ARMRepresentation => collector.addStatement(
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
                    case X86Representation => collector.addStatement(
                        List(
                            PushInstr(List(r0)),
                            LoadInstr(r0, r0, ImmOffset(0)),
                            MoveInstr(r4, RegOp(r0)),
                            BranchLinkInstr("free"),
                            MoveInstr(r0, ImmOffset(getTypeSize(pe.typeId.get.getType()))),
                            MoveInstr(r4, RegOp(r0)),
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
}
