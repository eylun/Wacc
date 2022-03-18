import Helpers._
import Condition._
import constants._

object transRHS {

    /** Adds a list of instructions evaluating the RHS of an assignment to the
      * Wacc Buffer collector
      */
    def apply(rhs: AssignRHSNode, stackFrame: StackFrame)
        (implicit collector: WaccBuffer, repr: Representation): Unit = {
        rhs match {
            /** EXPRESSION NODE */
            case e: ExprNode => transExpression(e, stackFrame)
            /** ARRAY-LITER NODE */
            case al @ ArrayLiterNode(es) => {
                /** Add appropriate move instruction based on the array type */
                al.typeId.get match {
                    case AnyType() => {
                        collector.addStatement(
                          List(
                            MoveInstr(r0, ImmOffset(WORD_SIZE))
                          )
                        )
                    }
                    case a @ ArrayType(_, _, _) => {
                        collector.addStatement(
                            List(MoveInstr(r0, ImmOffset(getArraySize(a, es.length))))
                        )
                    }
                    case _ =>
                }

                repr match {
                    case X86Representation => collector.addStatement(List(
                        MoveInstr(r4, RegOp(r0))
                    ))
                    case _ =>
                }
                
                collector.addStatement(List(
                    BranchLinkInstr("malloc", Condition.AL),
                    MoveInstr(r3, RegOp(r0))
                ))

                var ofs = WORD_SIZE
                repr match {
                    case X86Representation => ofs = 0
                    case _ => 
                }

                es.foreach { e => {
                    transExpression(e, stackFrame)
                    collector.addStatement(
                        List(
                        determineStoreInstr(e.typeId.get.getType(), r0, r3, ofs)
                        )
                    )
                    ofs += getTypeSize(e.typeId.get.getType())
                }}

                repr match {
                    case ARMRepresentation => collector.addStatement(List(
                        MoveInstr(r0, ImmOffset(es.length)),
                        StoreInstr(r0, r3, ImmOffset(0)),
                        MoveInstr(r0, RegOp(r3))
                    ))
                    case X86Representation => collector.addStatement(List(
                        MoveInstr(r0, RegOp(r3))
                    ))
                }
            }
            /** NEW PAIR NODE */
            case NewPairNode(e1, e2) => {
                /** Evaluate the pair-elem expressions and stores it in the
                  * stack
                  */
                addNewPairElem(e1, stackFrame)
                addNewPairElem(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(r0, ImmOffset(8)),
                    BranchLinkInstr("malloc", AL),
                    PopInstr(List(r1, r2)),
                    StoreInstr(r2, r0, ImmOffset(0), false),
                    StoreInstr(r1, r0, ImmOffset(4), false)
                  )
                )
                stackFrame.dropTempOffset(WORD_SIZE * 2)
            }
            /** (FUNCTION) CALL NODE */
            case CallNode(i, args) => {
                /** Look up function Id from the stack frame */
                val FunctionId(t, plist, _) = stackFrame.currST.lookupAll(i.s).get
                var offset = 0

                /** Push params into stack */
                (plist zip args).reverse.foreach {
                    case (p, e) => {
                        transExpression(e, stackFrame)
                        collector.addStatement(List(p match {
                            case Param(CharType()) | Param(BoolType()) => {
                                offset += BIT_SIZE
                                stackFrame.addTempOffset(BIT_SIZE)
                                StoreByteInstr(r0, sp, ImmOffset(-BIT_SIZE), true)
                            }
                            case _ => {
                                offset += WORD_SIZE
                                stackFrame.addTempOffset(WORD_SIZE)
                                StoreInstr(r0, sp, ImmOffset(-WORD_SIZE), true)
                            }
                        }))
                    }
                }

                /** Branch to function and recover stack */
                collector.addStatement(
                  List(
                    BranchLinkInstr(s"f_${i.s}", Condition.AL)
                  )
                )
                offset match {
                    case 0 =>
                    case _ =>
                        collector.addStatement(
                          List(AddInstr(sp, sp, ImmOffset(offset), false))
                        )
                }
                stackFrame.dropTempOffset(offset)

            }
            /** PAIR ELEM NODE */
            case e: PairElemNode => {
                /** Include null pointer check */
                collector.insertUtil(UtilFlag.PCheckNullPointer)
                collector.addStatement(
                  List(
                    BranchLinkInstr("p_check_null_pointer")
                  )
                      ++
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
                                    LoadInstr(r0, r0, ImmOffset(WORD_SIZE))
                                  )
                              }
                          })
                )

                collector.addStatement(
                  List(determineLoadInstr(e.typeId.get.getType(), r0, r0, 0))
                )
            }
        }
    }
}
