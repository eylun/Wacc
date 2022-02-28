import Helpers._
import Helpers.UtilFlag._
import constants._
import javax.management.RuntimeErrorException

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
                        val ArrayLiterNode(es) = r
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
                            BranchLinkInstr("malloc", Condition.AL),
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
            case ite @ IfThenElseNode(e, s1, s2) => {
                val labelFalse = s"ite_${collector.tickIte()}"
                val labelTrue = s"ite_${collector.tickIte()}"
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(
                    CompareInstr(Reg(0), ImmOffset(0), Condition.AL),
                    BranchInstr(labelFalse, Condition.EQ)
                  )
                )
                transStatement(
                  s1,
                  stackFrame.join(StackFrame(ite.trueST), ite.trueST)
                )
                collector.addStatement(
                  List(BranchInstr(labelTrue, Condition.AL), Label(labelFalse))
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
                  List(
                    BranchInstr(labelCheck, Condition.AL),
                    Label(labelContent)
                  )
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
            case FreeNode(e)   =>
            case ReturnNode(e) =>
            case ReadNode(l)   =>
            case StatListNode(_) =>
                throw new RuntimeException("Invalid Statement List Node")
        }
        collector.addStatement(stackFrame.tail)
    }

    def printExpr(e: ExprNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        e match {
            case IntLiterNode(_) | Neg(_) | Len(_) | Ord(_) | Mult(_, _) |
                Div(_, _) | Mod(_, _) | Add(_, _) | Sub(_, _) => {

                /** Call transExpression and branch */
                transExpression(e, stackFrame)

                collector.insertUtil(PPrintInt)

                /** Add branch instruction Statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_int"))
                )
            }
            case BoolLiterNode(_) | Not(_) | GT(_, _) | GTE(_, _) | LT(_, _) |
                LTE(_, _) | Equal(_, _) | NotEqual(_, _) | And(_, _) |
                Or(_, _) => {

                /** Call transExpression and branch */
                transExpression(e, stackFrame)
                collector.insertUtil(PPrintBool)

                /** Add branch instruction Statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_bool"))
                )
            }
            case CharLiterNode(_) | Chr(_) => {
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(BranchLinkInstr("putchar"))
                )
            }

            case StringLiterNode(_) => {

                /** call transExpression and branch */
                transExpression(e, stackFrame)
                collector.insertUtil(PPrintString)

                /** Add branch instruction statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_string"))
                )

            }

            case PairLiterNode() => {
                transExpression(e, stackFrame)
                collector.insertUtil(PPrintRef)

                /** Add branch instruction statement */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_reference"))
                )
            }

            case IdentNode(s) => {

                /** Loads sp into Reg(0) */
                transExpression(e, stackFrame)

                /** Get Ident Node Type */
                val nodeType: Type =
                    (stackFrame.st.lookupAll(s)).get.getType()

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
                        collector.insertUtil(UtilFlag.PPrintChar)
                    }
                    case StringType() => {
                        collector.insertUtil(UtilFlag.PPrintString)

                        /** Add branch instruction statement */
                        collector.addStatement(
                          List(BranchLinkInstr("p_print_string"))
                        )
                    }
                    case ArrayType(CharType(), _, _) => {
                        collector.insertUtil(UtilFlag.PPrintString)

                        /** Add branch instruction statement */
                        collector.addStatement(
                          List(BranchLinkInstr("p_print_string"))
                        )

                        collector.insertUtil(UtilFlag.PPrintNewLine)
                    }
                    case NullPairType() | PairType(_, _) |
                        ArrayType(_, _, _) => {
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
            case _ =>
                throw new RuntimeException(s"Invalid node $e in AST")
        }
    }
}
