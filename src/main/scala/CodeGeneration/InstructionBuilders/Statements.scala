import Helpers._
import Helpers.UtilFlag._
import constants._
import javax.management.RuntimeErrorException
import OptimisationFlag._

/** Evaluates a statement and adds the appropriate instructions into the wacc buffer
  */
object transStatement {
    def apply(statList: StatNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        val StatListNode(l) = statList
        l.foreach {
            /** THROW STATEMENT: throw <expr> */
            case ThrowNode(e) => {
                collector.insertUtil(UtilFlag.PExceptionError)
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(
                    LoadImmIntInstr(r7, typeNumConvert(e.typeId.get.getType())),
                    LoadLabelInstr(r2, "catch_address"),
                    LoadInstr(r2, r2, ImmOffset(0)),
                    CompareInstr(r2, ImmOffset(0)),
                    BranchInstr("p_exception_error", Condition.EQ),
                    MoveInstr(pc, RegOp(r2))
                  )
                )
            }
            /** TRY-CATCH STATEMENT: try <stat> catch (<type> <ident>) <stat> or catch (<type> <ident>) <stat>
              */
            case tc @ TryCatchNode(s, cs) => {
                val idx = collector.tickTryCatch()
                collector.insertUtil(UtilFlag.PExceptionError)

                /** ASM code for the Try block */
                collector.addStatement(
                  List(
                    /** Storing bss values onto the stack */
                    SubInstr(sp, sp, ImmOffset(8)),
                    LoadLabelInstr(r2, "prev_sp"),
                    LoadInstr(r2, r2, ImmOffset(0)),
                    StoreInstr(r2, sp, ImmOffset(4)),
                    LoadLabelInstr(r2, "catch_address"),
                    LoadInstr(r2, r2, ImmOffset(0)),
                    StoreInstr(r2, sp, ImmOffset(0)),
                    /** Loading bss values with new values */
                    LoadLabelInstr(r2, "prev_sp"),
                    StoreInstr(sp, r2, ImmOffset(0)),
                    LoadLabelInstr(r2, "catch_address"),
                    LoadLabelInstr(r3, s"catch_handler_$idx"),
                    StoreInstr(r3, r2, ImmOffset(0))
                  )
                )
                stackFrame.addTempOffset(8)
                val trySF = stackFrame.join(tc.tryST)
                collector.addStatement(trySF.head)
                transStatement(s, trySF)
                collector.addStatement(
                  trySF.tail ++:
                      List(
                        LoadInstr(r2, sp, ImmOffset(4)),
                        LoadLabelInstr(r3, "prev_sp"),
                        StoreInstr(r2, r3, ImmOffset(0)),
                        LoadInstr(r2, sp, ImmOffset(0)),
                        LoadLabelInstr(r3, "catch_address"),
                        StoreInstr(r2, r3, ImmOffset(0)),
                        AddInstr(sp, sp, ImmOffset(8)),
                        BranchInstr(s"catch_end_$idx")
                      )
                )

                /** ASM code for the catch handler */
                /** Reset sp, previous sp and catch address */
                collector.addStatement(
                  List(
                    Label(s"catch_handler_$idx"),
                    /** Reset sp value to previous sp */
                    LoadLabelInstr(r2, "prev_sp"),
                    LoadInstr(sp, r2, ImmOffset(0)),
                    /** Revert bss values to previous */
                    LoadInstr(r2, sp, ImmOffset(4)),
                    LoadLabelInstr(r3, "prev_sp"),
                    StoreInstr(r2, r3, ImmOffset(0)),
                    LoadInstr(r2, sp, ImmOffset(0)),
                    LoadLabelInstr(r3, "catch_address"),
                    StoreInstr(r2, r3, ImmOffset(0)),
                    AddInstr(sp, sp, ImmOffset(8))
                  )
                )
                stackFrame.dropTempOffset(8)

                /** ASM code for branching to appropriate catch statements */
                cs.foreach(c => {
                    val typeNum = typeNumConvert(c.t.typeId.get.getType())
                    collector.addStatement(
                      List(
                        CompareInstr(r7, ImmOffset(typeNum)),
                        BranchInstr(
                          s"catch_${c.t.typeId.get.getType()}_$idx",
                          Condition.EQ
                        )
                      )
                    )
                })
                /* ASM code for handling cases where nothing is caught */
                collector.addStatement(
                  List(
                    LoadLabelInstr(r2, "catch_address"),
                    LoadInstr(r2, r2, ImmOffset(0)),
                    CompareInstr(r2, ImmOffset(0)),
                    BranchInstr("p_exception_error", Condition.EQ),
                    MoveInstr(pc, RegOp(r2))
                  )
                )

                /** ASM code for the catch block(s) */
                cs.foreach(c => {

                    /** Add label for each catch block */
                    collector.addStatement(
                      Label(s"catch_${c.t.typeId.get.getType()}_$idx")
                    )

                    /** Insert the thrown value to the stack at the appropriate position
                      */
                    c.t.typeId.get.getType() match {
                        case BoolType() | CharType() =>
                            collector.addStatement(
                              StoreByteInstr(r0, sp, ImmOffset(-1))
                            )
                        case _ =>
                            collector.addStatement(
                              StoreInstr(r0, sp, ImmOffset(-4))
                            )
                    }

                    /** Translate statement within each catch block */
                    val cSF = stackFrame.join(c.catchST)
                    cSF.unlock(c.i.s)
                    collector.addStatement(cSF.head)
                    transStatement(c.s, cSF)
                    collector.addStatement(
                      cSF.tail :+ BranchInstr(s"catch_end_$idx")
                    )
                })

                /** Label to jump to to exit try-catch */
                collector.addStatement(Label(s"catch_end_$idx"))
            }
            /** NEW ASSIGNMENT STATEMENT: <type> <ident> '=' <Assign-RHS> */
            case NewAssignNode(t, i, r) => {

                /** Evaluate the RHS of the assignment */
                transRHS(r, stackFrame, i.s)
                stackFrame.unlock(i.s)

                /** Store instruction generated via determineStoreInstr() */
                collector.addStatement(
                  List(
                    determineStoreInstr(
                      t.typeId.get.getType(),
                      r0,
                      sp,
                      stackFrame.getOffset(i.s)
                    )
                  )
                )
            }
            /** LR ASSIGNMENT STATEMENT: <Assign-LHS> '=' <Assign-RHS> */
            case LRAssignNode(l, r) => {
                l match {
                    case IdentNode(s) => {

                        /** Removes identifier from (parent)constant map if removeConstants flag is set */
                        if (collector.optFlag == OptimisationFlag.Oph & stackFrame.currST.rmParentConst) {
                            stackFrame.currST.encSymTable.get.removeConstantVar(s)
                        }

                        /** Generates instructions for LHS and RHS */
                        transRHS(r, stackFrame, s)
                        transLHS(l, stackFrame)

                        collector.addStatement(
                          List(
                            determineStoreInstr(
                              stackFrame.currST.lookupAll(s).get.getType(),
                              r0,
                              sp,
                              stackFrame.getOffset(s)
                            )
                          )
                        )
                    }
                    case ae @ ArrayElemNode(IdentNode(s), es) => {

                        /** Generates instructions for LHS and RHS */
                        transRHS(r, stackFrame, s)
                        transLHS(l, stackFrame)

                        collector.addStatement(
                          List(
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
                        transRHS(r, stackFrame)
                        transLHS(l, stackFrame)
                        collector.addStatement(
                          List(
                            determineStoreInstr(
                              l.typeId.get.getType(),
                              r0,
                              r1,
                              0
                            )
                          )
                        )
                    }
                }
            }
            /** IF THEN ELSE STATEMENT: ‘if’ ⟨expr ⟩ ‘then’ ⟨stat⟩ ‘else’ ⟨stat⟩ ‘fi’
              */
            case ite @ IfThenElseNode(e, s1, s2) => {

                /** Labels for True and False cases */
                val labelFalse = s"ite_${collector.tickIte()}"
                val labelTrue = s"ite_${collector.tickIte()}"

                /** Constant prop: clear constants */
                ite.trueST.removeConstFromParent()
                ite.falseST.removeConstFromParent()

                /** Stack frames */
                val trueSF = stackFrame.join(ite.trueST)
                val falseSF =
                    stackFrame.join(ite.falseST)

                /** Evaluate conditional expression and branch accordingly */
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(
                    CompareInstr(r0, ImmOffset(0), Condition.AL),
                    BranchInstr(labelFalse, Condition.EQ)
                  ) ++ trueSF.head
                )
                transStatement(s1, trueSF)

                collector.addStatement(
                  trueSF.tail ++
                      List(
                        BranchInstr(labelTrue, Condition.AL),
                        Label(labelFalse)
                      )
                      ++ falseSF.head
                )
                transStatement(s2, falseSF)
                collector.addStatement(falseSF.tail ++ List(Label(labelTrue)))

                /** Constant Propogation: remove variables that are being assigned in the loop body */
                ite.trueST.dict.keys.foreach(ident => stackFrame.currST.removeConstantVar(ident))
            }
            /** BEGIN-END STATEMENT: ‘begin’ ⟨stat⟩ ‘end’ */
            case be @ BeginEndNode(s) => {

                /** Set up new stack frame using the node's symbol table */
                val beSF = stackFrame.join(
                  be.newScopeST
                )

                /** Constant Propogation: set flag so any assigned identifier in scope will be removed from the map of
                  * constants
                  */
                beSF.currST.removeConstFromParent()

                collector.addStatement(beSF.head)
                transStatement(s, beSF)
                collector.addStatement(beSF.tail)
            }
            /** DO WHILE STATEMENT: ‘while’ ⟨expr ⟩ ‘do’ ⟨stat⟩ ‘done’ */
            case wd @ WhileDoNode(e, s) => {

                /** Label for instructions for do-while loop body */
                val labelContent = s"wd_${collector.tickWd()}"

                /** Label for do-while loop conditional */
                val labelCheck = s"wd_${collector.tickWd()}"
                collector.addStatement(
                  List(
                    BranchInstr(labelCheck, Condition.AL),
                    Label(labelContent)
                  )
                )

                /** Set up stack frame */
                val wdSF = stackFrame.join(
                  wd.newScopeST
                )

                /** Constant Propogation: if a constant was assigned, remove it from the parent ST */
                wdSF.currST.removeConstFromParent()

                collector.addStatement(wdSF.head)
                transStatement(s, wdSF)
                collector.addStatement(wdSF.tail)

                /** Branches to loop body instructions if condition is true
                  */
                collector.addStatement(List(Label(labelCheck)))

                transExpression(e, stackFrame)

                collector.addStatement(
                  (
                    List(
                      CompareInstr(r0, ImmOffset(1), Condition.AL),
                      BranchInstr(labelContent, Condition.EQ)
                    )
                  )
                )
            }
            /** SKIP STATEMENT: ‘skip’ */
            case SkipNode() =>
            /** EXIT STATEMENT: ‘exit’ ⟨expr ⟩ */
            case ExitNode(e) => {

                /** Evaluates expression and Branches */
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(
                    BranchLinkInstr("exit", Condition.AL)
                  )
                )
            }
            /** PRINT STATEMENT: ‘print’ ⟨expr ⟩ */
            case PrintNode(e) => {

                /** Calls printExpr helper function */
                printExpr(e, stackFrame)
            }
            /** PRINTLN STATEMENT: ‘println’ ⟨expr ⟩ */
            case PrintlnNode(e) => {

                /** Calls printExpr helper function */
                printExpr(e, stackFrame)

                /** Inserts instructions for p_print_ln */
                collector.insertUtil(PPrintNewLine)

                /** Branch instruction to p_print_ln */
                collector.addStatement(List(BranchLinkInstr("p_print_ln")))
            }
            /** FREE STATEMENT: ‘free’ ⟨expr ⟩ */
            case FreeNode(e) => {
                transExpression(e, stackFrame)
                e.typeId.get.getType() match {
                    case ArrayType(_, _, _) => {
                        collector.addStatement(
                          List(
                            BranchLinkInstr("free")
                          )
                        )
                    }
                    case PairType(_, _) => {
                        collector.insertUtil(UtilFlag.PFreePair)
                        collector.addStatement(
                          List(BranchLinkInstr("p_free_pair"))
                        )
                    }
                    case _ =>
                        throw new RuntimeException(
                          "Invalid Target Type for Free Statement"
                        )
                }
            }
            /** RETURN STATEMENT: ‘return’ ⟨expr ⟩ */
            case ReturnNode(e) => {
                transExpression(e, stackFrame)
                collector.addStatement(
                  stackFrame.returnTail ++ List(PopInstr(List(pc)))
                )
            }
            /** READ STATEMENT: ‘read’ ⟨assign-lhs⟩ */
            case ReadNode(l) => {

                /** transLHS generates instructions for the assign-lhs node */
                transLHS(l, stackFrame)

                l match {
                    case IdentNode(s) => {

                        /** Reassign variable: remove from constant propogation map */
                        stackFrame.currST.removeConstantVar(s)

                        collector.addStatement(
                          List(
                            AddInstr(
                              r0,
                              sp,
                              ImmOffset(stackFrame.getOffset(s)),
                              false
                            )
                          )
                        )
                    }
                    case _ =>
                        collector.addStatement(
                          List(
                            AddInstr(
                              r0,
                              r1,
                              ImmOffset(0),
                              false
                            )
                          )
                        )
                }

                /** Throws an error for input not of char or int type */
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
                        throw new RuntimeException(
                          "Invalid Target Type for Read Statement"
                        )
                }

            }
            case StatListNode(_) =>
                throw new RuntimeException("Invalid Statement List Node")
        }
    }

    /** Helper function for print and println. */
    def printExpr(e: ExprNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {

        /** Evaluate the expression node */
        transExpression(e, stackFrame)

        /** Insert the instruction sequence corresponding to the Expression Node's type and a branch link instruction to
          * the inserted sequence
          */
        e match {
            case IntLiterNode(_) | Neg(_) | Len(_) | Ord(_) | Mult(_, _) | Div(_, _) | Mod(_, _) | Add(_, _) |
                Sub(_, _) => {

                /** Insert instruction sequence for printing integers */
                collector.insertUtil(PPrintInt)

                /** Branch Instruction */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_int"))
                )
            }
            case BoolLiterNode(_) | Not(_) | GT(_, _) | GTE(_, _) | LT(_, _) | LTE(_, _) | Equal(_, _) |
                NotEqual(_, _) | And(_, _) | Or(_, _) => {

                /** Insert instruction sequence for printing booleans */
                collector.insertUtil(PPrintBool)

                /** Branch Instruction */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_bool"))
                )
            }
            case CharLiterNode(_) | Chr(_) | SCharAtNode(_, _) => {
                collector.addStatement(
                  List(BranchLinkInstr("putchar"))
                )
            }

            case StringLiterNode(_) => {

                /** Insert instruction sequence for printing strings */
                collector.insertUtil(PPrintString)

                /** Branch Instruction */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_string"))
                )

            }

            case PairLiterNode() => {

                /** Insert instruction sequence for printing references */
                collector.insertUtil(PPrintRef)

                /** Branch Instruction */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_reference"))
                )
            }

            case IdentNode(s) => {

                /** Get Ident Node Type */
                val nodeType: Type =
                    (stackFrame.currST.lookupAll(s)).get.getType()

                /** Insert instruction sequence corresponding to the type of data stored in the identifier
                  */
                determinePrintType(nodeType)
            }
            case ArrayElemNode(IdentNode(s), es) => {

                /** Get array type and dimension. */
                val ArrayType(nodeType, _, dimension) =
                    (stackFrame.currST.lookupAll(s)).get.getType()

                /** If the number of indexes provided matches the dimension, the type of expression to be printed
                  * coresponds to the nodeType of the array (Eg. Given int[] a = [[1,2],[3,4]], then a[0][1] is of type
                  * Int)
                  */
                es.length match {
                    case `dimension` => determinePrintType(nodeType)
                    case _ => {

                        /** Array element to be printed is an array itself. Hence, we print the reference to the array.
                          * (Eg. Given int[] a = [[1,2],[3,4]], then a[0] is an array)
                          */
                        collector.insertUtil(UtilFlag.PPrintRef)

                        /** Branch Instruction */
                        collector.addStatement(
                          List(BranchLinkInstr("p_print_reference"))
                        )
                    }
                }
            }
            case _ => throw new RuntimeException(s"Invalid node $e in AST")
        }
    }

    /** Given a type, it generates the appropriate instruction sequence and corresponding branch instruction
      */
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
            case StringType() | ExceptionType() => {
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
