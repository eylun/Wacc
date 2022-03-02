import Helpers._
import Helpers.UtilFlag._
import constants._
import javax.management.RuntimeErrorException

object transStatement {
    def apply(statList: StatNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        val StatListNode(l) = statList
        l.foreach {
            case NewAssignNode(t, i, r) => {
                transRHS(r, stackFrame)
                t match {
                    case CharTypeNode() | BoolTypeNode() => {
                        collector.addStatement(
                          List(
                            StoreByteInstr(
                              Reg(0),
                              sp,
                              ImmOffset(stackFrame.getOffset(i.s))
                            )
                          )
                        )
                    }
                    case _ =>
                        collector.addStatement(
                          List(
                            StoreInstr(
                              Reg(0),
                              sp,
                              ImmOffset(stackFrame.getOffset(i.s))
                            )
                          )
                        )
                }
                // all new assign code ends with storing reg 0 into stack
            }
            case LRAssignNode(l, r) => {
                l match {
                    case IdentNode(s) => {
                        transRHS(r, stackFrame)
                        collector.addStatement(
                          List(
                            determineStoreInstr(
                              stackFrame.st.lookupAll(s).get.getType(),
                              r0,
                              sp,
                              stackFrame.getOffset(s)
                            )
                          )
                        )
                    }
                    case ae @ ArrayElemNode(IdentNode(s), es) => {
                        collector.insertUtil(UtilFlag.PCheckArrayBounds)
                        transRHS(r, stackFrame)
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
                            PopInstr(List(Reg(0), Reg(4))),
                            determineStoreInstr(
                              ae.typeId.get.getType(),
                              r0,
                              r1,
                              0
                            )
                          )
                        )

                    }
                    case l: PairElemNode => {

                        collector.insertUtil(UtilFlag.PCheckNullPointer)
                        transRHS(r, stackFrame)
                        stackFrame.addTempOffset(WORD_SIZE)
                        collector.addStatement(
                          List(
                            PushInstr(List(r0))
                          )
                        )
                        l match {
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
                        collector.addStatement(
                          List(
                            PushInstr(List(r0)),
                            LoadInstr(r0, r0, ImmOffset(0)),
                            BranchLinkInstr("free"),
                            MoveInstr(
                              r0,
                              ImmOffset(getTypeSize(l.typeId.get.getType()))
                            ),
                            BranchLinkInstr("malloc"),
                            PopInstr(List(r1)),
                            StoreInstr(r0, r1, ImmOffset(0)),
                            MoveInstr(r1, RegOp(r0)),
                            PopInstr(List(r0)),
                            determineStoreInstr(
                              l.typeId.get.getType(),
                              r0,
                              r1,
                              0
                            )
                          )
                        )
                        stackFrame.dropTempOffset(WORD_SIZE)
                    }
                }
            }
            case ite @ IfThenElseNode(e, s1, s2) => {
                val labelFalse = s"ite_${collector.tickIte()}"
                val labelTrue = s"ite_${collector.tickIte()}"
                val trueSF = stackFrame.join(StackFrame(ite.trueST), ite.trueST)
                val falseSF =
                    stackFrame.join(StackFrame(ite.falseST), ite.falseST)
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(
                    CompareInstr(Reg(0), ImmOffset(0), Condition.AL),
                    BranchInstr(labelFalse, Condition.EQ)
                  ) ++ trueSF.head
                )
                transStatement(
                  s1,
                  trueSF
                )
                collector.addStatement(
                  trueSF.tail ++
                      List(
                        BranchInstr(labelTrue, Condition.AL),
                        Label(labelFalse)
                      )
                      ++ falseSF.head
                )
                transStatement(
                  s2,
                  falseSF
                )
                collector.addStatement(falseSF.tail ++ List(Label(labelTrue)))
            }
            case be @ BeginEndNode(s) => {
                val beSF = stackFrame.join(
                  StackFrame(be.newScopeST),
                  be.newScopeST
                )
                collector.addStatement(beSF.head)
                transStatement(
                  s,
                  beSF
                )
                collector.addStatement(beSF.tail)
            }
            case wd @ WhileDoNode(e, s) => {
                val labelContent = s"wd_${collector.tickWd()}"
                val labelCheck = s"wd_${collector.tickWd()}"
                collector.addStatement(
                  List(
                    BranchInstr(labelCheck, Condition.AL),
                    Label(labelContent)
                  )
                )
                val wdSF = stackFrame.join(
                  StackFrame(wd.newScopeST),
                  wd.newScopeST
                )
                collector.addStatement(wdSF.head)
                transStatement(
                  s,
                  wdSF
                )
                collector.addStatement(wdSF.tail)

                collector.addStatement(List(Label(labelCheck)))
                transExpression(e, stackFrame)
                collector.addStatement(
                  (
                    List(
                      CompareInstr(Reg(0), ImmOffset(1), Condition.AL),
                      BranchInstr(labelContent, Condition.EQ)
                    )
                  )
                )
            }
            case SkipNode() =>
            case ExitNode(e) => {
                transExpression(e, stackFrame)
                collector.addStatement(
                  stackFrame.tail ++
                      List(
                        BranchLinkInstr("exit", Condition.AL)
                      )
                )
            }
            case PrintNode(e) => {
                printExpr(e, stackFrame)
            }

            case PrintlnNode(e) => {
                printExpr(e, stackFrame)
                collector.insertUtil(PPrintNewLine)

                /** Add branch instruction Statement */
                collector.addStatement(List(BranchLinkInstr("p_print_ln")))
            }
            case FreeNode(e) =>
            case ReturnNode(e) => {
                transExpression(e, stackFrame)
                collector.addStatement(
                  stackFrame.tail ++ List(PopInstr(List(pc)))
                )
            }
            case ReadNode(l) => {

                collector.addStatement(
                  List(AddInstr(Reg(0), sp, ImmOffset(0), false))
                )

                l.typeId.get.getType() match {
                    case CharType() => {
                        collector.insertUtil(UtilFlag.PReadChar)
                        collector.addStatement(
                          List(
                            BranchLinkInstr("p_read_char")
                          )
                        )
                    }

                    case IntType() => {
                        collector.insertUtil(UtilFlag.PReadInt)
                        collector.addStatement(
                          List(
                            BranchLinkInstr("p_read_int")
                          )
                        )
                    }

                    case _ =>
                        // Note: Read statement only takes character or int input
                        throw new RuntimeException(
                          "Invalid Target Type for Read Statement"
                        )
                }

            }
            case StatListNode(_) =>
                throw new RuntimeException("Invalid Statement List Node")
        }
    }

    def printExpr(e: ExprNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {

        /** Call transExpression */
        transExpression(e, stackFrame)
        e match {
            case IntLiterNode(_) | Neg(_) | Len(_) | Ord(_) | Mult(_, _) |
                Div(_, _) | Mod(_, _) | Add(_, _) | Sub(_, _) => {

                /** Call branch */
                collector.insertUtil(PPrintInt)

                /** Add branch instruction Statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_int"))
                )
            }
            case BoolLiterNode(_) | Not(_) | GT(_, _) | GTE(_, _) | LT(_, _) |
                LTE(_, _) | Equal(_, _) | NotEqual(_, _) | And(_, _) |
                Or(_, _) => {

                /** Call branch */
                collector.insertUtil(PPrintBool)

                /** Add branch instruction Statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_bool"))
                )
            }
            case CharLiterNode(_) | Chr(_) => {
                collector.addStatement(
                  List(BranchLinkInstr("putchar"))
                )
            }

            case StringLiterNode(_) => {

                /** Call branch */
                collector.insertUtil(PPrintString)

                /** Add branch instruction statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_string"))
                )

            }

            case PairLiterNode() => {
                collector.insertUtil(PPrintRef)

                /** Add branch instruction statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_reference"))
                )
            }

            case IdentNode(s) => {

                /** Get Ident Node Type */
                val nodeType: Type =
                    (stackFrame.st.lookupAll(s)).get.getType()

                determinePrintType(nodeType)
            }
            case ArrayElemNode(IdentNode(s), es) => {
                val ArrayType(nodeType, _, dimension) =
                    (stackFrame.st.lookupAll(s)).get.getType()

                es.length match {
                    case `dimension` => determinePrintType(nodeType)
                    case _ => {
                        collector.insertUtil(UtilFlag.PPrintRef)

                        /** Add branch instruction statement */
                        collector.addStatement(
                          List(BranchLinkInstr("p_print_reference"))
                        )
                    }
                }
            }
            case _ => throw new RuntimeException(s"Invalid node $e in AST")
        }
    }

    def determinePrintType(nodeType: Type)(implicit collector: WaccBuffer) = {
        nodeType match {
            case IntType() => {
                collector.insertUtil(UtilFlag.PPrintInt)

                /** Add branch instruction Statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_int"))
                )
            }
            case BoolType() => {
                collector.insertUtil(UtilFlag.PPrintBool)

                /** Add branch instruction Statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_bool"))
                )
            }
            case CharType() => {
                collector.addStatement(
                  List(BranchLinkInstr("putchar"))
                )
            }
            case StringType() => {
                collector.insertUtil(UtilFlag.PPrintString)

                /** Add branch instruction statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_string"))
                )
            }
            case ArrayType(CharType(), _, 1) => {
                collector.insertUtil(UtilFlag.PPrintString)

                /** Add branch instruction statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_string"))
                )

                collector.insertUtil(UtilFlag.PPrintNewLine)
            }
            case NullPairType() | PairType(_, _) | ArrayType(_, _, _) => {
                collector.insertUtil(UtilFlag.PPrintRef)

                /** Add branch instruction statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_reference"))
                )
            }
            case _ =>
                throw new RuntimeException("Invalid Identifier")
        }

    }
}
