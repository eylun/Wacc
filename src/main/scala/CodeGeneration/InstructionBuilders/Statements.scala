import Helpers._
import Helpers.UtilFlag._
import constants._
import javax.management.RuntimeErrorException

/** Evaluates a statement and adds the appropriate instructions into the wacc
  * buffer
  */
object transStatement {
    def apply(statList: StatNode, stackFrame: StackFrame)(implicit
        collector: WaccBuffer
    ): Unit = {
        val StatListNode(l) = statList
        l.foreach {
            /** NEW ASSIGNMENT STATEMENT: <type> <ident> '=' <Assign-RHS> */
            case NewAssignNode(t, i, r) => {

                /** Evaluate the RHS of the assignment */
                transRHS(r, stackFrame)
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

                        /** Generates instructions for LHS and RHS */
                        transRHS(r, stackFrame)
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
                        transRHS(r, stackFrame)
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
            /** IF THEN ELSE STATEMENT: ‘if’ ⟨expr ⟩ ‘then’ ⟨stat⟩ ‘else’ ⟨stat⟩
              * ‘fi’
              */
            case ite @ IfThenElseNode(e, s1, s2) => {

                /** Labels for True and False cases */
                val labelFalse = s"ite_${collector.tickIte()}"
                val labelTrue = s"ite_${collector.tickIte()}"

                /** Stack frames */
                val trueSF = stackFrame.join(StackFrame(ite.trueST), ite.trueST)
                val falseSF =
                    stackFrame.join(StackFrame(ite.falseST), ite.falseST)

                /** Evaluate conditional expression and branch accordingly */
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(
                    CompareInstr(r0, ImmOffset(0), Condition.AL),
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
            /** BEGIN-END STATEMENT: ‘begin’ ⟨stat⟩ ‘end’ */
            case be @ BeginEndNode(s) => {

                /** Set up new stack frame using the node's symbol table */
                val beSF = stackFrame.join(
                  be.newScopeST
                )
                collector.addStatement(beSF.head)
                transStatement(
                  s,
                  beSF
                )
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
                collector.addStatement(wdSF.head)
                transStatement(
                  s,
                  wdSF
                )
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
                collector.insertUtil(UtilFlag.PFreePair)
                collector.addStatement(List(BranchLinkInstr("p_free_pair")))
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

        /** Insert the instruction sequence corresponding to the Expression
          * Node's type and a branch link instruction to the inserted sequence
          */
        e match {
            case IntLiterNode(_) | Neg(_) | Len(_) | Ord(_) | Mult(_, _) |
                Div(_, _) | Mod(_, _) | Add(_, _) | Sub(_, _) => {

                /** Insert instruction sequence for printing integers */
                collector.insertUtil(PPrintInt)

                /** Branch Instruction */
                collector.addStatement(
                  List(BranchLinkInstr("p_print_int"))
                )
            }
            case BoolLiterNode(_) | Not(_) | GT(_, _) | GTE(_, _) | LT(_, _) |
                LTE(_, _) | Equal(_, _) | NotEqual(_, _) | And(_, _) |
                Or(_, _) => {

                /** Insert instruction sequence for printing booleans */
                collector.insertUtil(PPrintBool)

                /** Branch Instruction */
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

                /** Insert instruction sequence corresponding to the type of
                  * data stored in the identifier
                  */
                determinePrintType(nodeType)
            }
            case ArrayElemNode(IdentNode(s), es) => {

                /** Get array type and dimension. */
                val ArrayType(nodeType, _, dimension) =
                    (stackFrame.currST.lookupAll(s)).get.getType()

                /** If the number of indexes provided matches the dimension, the
                  * type of expression to be printed coresponds to the nodeType
                  * of the array (Eg. Given int[] a = [[1,2],[3,4]], then
                  * a[0][1] is of type Int)
                  */
                es.length match {
                    case `dimension` => determinePrintType(nodeType)
                    case _ => {

                        /** Array element to be printed is an array itself.
                          * Hence, we print the reference to the array. (Eg.
                          * Given int[] a = [[1,2],[3,4]], then a[0] is an
                          * array)
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

    /** Given a type, it generates the appropriate instruction sequence and
      * corresponding branch instruction
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
