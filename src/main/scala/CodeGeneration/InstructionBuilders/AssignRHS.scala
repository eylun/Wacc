import Helpers._
import Condition._
import constants._
import OptimisationFlag._

object transRHS {

    /** Adds a list of instructions evaluating the RHS of an assignment to the Wacc Buffer collector
      */
    def apply(rhs: AssignRHSNode, stackFrame: StackFrame, identString: String = "")(implicit
        collector: WaccBuffer
    ): Unit = {
        rhs match {
            /** EXPRESSION NODE */
            case e: ExprNode => transExpression(e, stackFrame, true, identString)

            /** ARRAY-LITER NODE */
            case al @ ArrayLiterNode(es) => {

                /** Add appriopriate move instruction based on the array type */
                al.typeId.get match {
                    case AnyType() => {
                        collector.addStatement(
                          List(
                            MoveInstr(
                              r0,
                              ImmOffset(WORD_SIZE)
                            )
                          )
                        )
                    }
                    case a @ ArrayType(_, _, _) => {
                        collector.addStatement(
                          List(
                            MoveInstr(
                              r0,
                              ImmOffset(getArraySize(a, es.length))
                            )
                          )
                        )
                    }
                    case _ =>
                }
                collector.addStatement(
                  List(
                    BranchLinkInstr("malloc", Condition.AL),
                    MoveInstr(r3, RegOp(r0))
                  )
                )
                var ofs = WORD_SIZE
                es.foreach { e =>
                    {
                        transExpression(e, stackFrame)
                        collector.addStatement(
                          List(
                            determineStoreInstr(
                              e.typeId.get.getType(),
                              r0,
                              r3,
                              ofs
                            )
                          )
                        )
                        ofs += getTypeSize(e.typeId.get.getType())
                    }
                }
                collector.addStatement(
                  List(
                    MoveInstr(
                      r0,
                      ImmOffset(es.length)
                    ),
                    StoreInstr(
                      r0,
                      r3,
                      ImmOffset(0)
                    ),
                    MoveInstr(
                      r0,
                      RegOp(
                        r3
                      )
                    )
                  )
                )
            }
            /** NEW PAIR NODE */
            case NewPairNode(e1, e2) => {

                /** Evaluate the pair-elem expressions and stores it in the stack
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
                val FunctionId(t, plist, _) =
                    stackFrame.currST.lookupAll(i.s).get
                var offset = 0

                /** Push params into stack */
                (plist zip args).reverse.foreach {
                    case (p, e) => {
                        transExpression(e, stackFrame)
                        collector.addStatement(List(p match {
                            case Param(CharType()) | Param(BoolType()) => {
                                offset += BIT_SIZE
                                stackFrame.addTempOffset(BIT_SIZE)
                                StoreByteInstr(
                                  r0,
                                  sp,
                                  ImmOffset(-BIT_SIZE),
                                  true
                                )
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
            case FoldNode(i, e1, e2) => {
                val idx = collector.tickGeneral()
                val FunctionId(retType, ps, funcST) = stackFrame.currST.lookupAll(i.s).get
                val a @ ArrayType(elemType, _, dimension) = e2.typeId.get.getType()
                val elemSize = dimension match {
                    case 1 => getTypeSize(elemType)
                    case _ => WORD_SIZE
                }
                val retSize = getTypeSize(retType)
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(r3, RegOp(r0)),
                    /** Extract length out into r1 */
                    LoadInstr(r1, r3, ImmOffset(0)),
                    /** Increment both new and old array address by 4 to get to elements */
                    AddInstr(r3, r3, ImmOffset(4))
                  )
                )
                transExpression(e1, stackFrame)
                collector.addStatement(
                  List(
                    BranchInstr(s"Fold_cond_$idx"),
                    Label(s"Fold_$idx"),
                    PushInstr(List(r1, r3))
                  )
                )
                elemSize match {
                    case WORD_SIZE =>
                        collector.addStatement(
                          List(
                            StoreInstr(r0, sp, ImmOffset(-4), true),
                            LoadInstr(r0, r3, ImmOffset(0)),
                            StoreInstr(r0, sp, ImmOffset(-4), true)
                          )
                        )
                    case _ =>
                        collector.addStatement(
                          List(
                            StoreByteInstr(r0, sp, ImmOffset(-1), true),
                            LoadRegSignedByte(r0, r3, ImmOffset(0)),
                            StoreByteInstr(r0, sp, ImmOffset(-1), true)
                          )
                        )
                }
                collector.addStatement(
                  List(
                    BranchLinkInstr(s"f_${i.s}"),
                    AddInstr(sp, sp, ImmOffset(elemSize * 2)),
                    PopInstr(List(r1, r3)),
                    AddInstr(r3, r3, ImmOffset(elemSize)),
                    SubInstr(r1, r1, ImmOffset(1)),
                    Label(s"Fold_cond_$idx"),
                    CompareInstr(r1, ImmOffset(0)),
                    BranchInstr(s"Fold_$idx", Condition.NE)
                  )
                )
            }
            case MapNode(i, e) => {
                val idx = collector.tickGeneral()
                val FunctionId(retType, ps, funcST) = stackFrame.currST.lookupAll(i.s).get
                val a @ ArrayType(elemType, _, dimension) = e.typeId.get.getType()
                val elemSize = dimension match {
                    case 1 => getTypeSize(elemType)
                    case _ => WORD_SIZE
                }
                val retSize = getTypeSize(retType)
                transExpression(e, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(r3, RegOp(r0)),
                    LoadInstr(r4, r3, ImmOffset(0)),
                    LoadImmIntInstr(r0, retSize),
                    SMullInstr(r0, r4, r0, r4),
                    AddInstr(r0, r0, ImmOffset(4)),
                    PushInstr(List(r3)),
                    BranchLinkInstr("malloc"),
                    PopInstr(List(r3)),
                    SubInstr(sp, sp, ImmOffset(4)),
                    StoreInstr(r0, sp, ImmOffset(0)),
                    LoadInstr(r1, r3, ImmOffset(0)),
                    StoreInstr(r1, r0, ImmOffset(0)),
                    AddInstr(r3, r3, ImmOffset(4)),
                    AddInstr(r0, r0, ImmOffset(4)),
                    MoveInstr(r2, RegOp(r0)),
                    BranchInstr(s"Map_cond_$idx"),
                    Label(s"Map_$idx"),
                    PushInstr(List(r1, r2, r3))
                  )
                )
                elemSize match {
                    case WORD_SIZE =>
                        collector.addStatement(
                          List(
                            LoadInstr(r0, r3, ImmOffset(0)),
                            StoreInstr(r0, sp, ImmOffset(-4), true)
                          )
                        )
                    case _ =>
                        collector.addStatement(
                          List(
                            LoadRegSignedByte(r0, r3, ImmOffset(0)),
                            StoreByteInstr(r0, sp, ImmOffset(-1), true)
                          )
                        )
                }
                collector.addStatement(
                  List(
                    BranchLinkInstr(s"f_${i.s}"),
                    AddInstr(sp, sp, ImmOffset(elemSize)),
                    PopInstr(List(r1, r2, r3)),
                    determineStoreInstr(retType, r0, r2, 0),
                    AddInstr(r2, r2, ImmOffset(retSize)),
                    AddInstr(r3, r3, ImmOffset(elemSize)),
                    SubInstr(r1, r1, ImmOffset(1)),
                    Label(s"Map_cond_$idx"),
                    CompareInstr(r1, ImmOffset(0)),
                    BranchInstr(s"Map_$idx", Condition.NE),
                    LoadInstr(r0, sp, ImmOffset(0)),
                    AddInstr(sp, sp, ImmOffset(4))
                  )
                )
            }
            case ScanNode(i, e1, e2) => {
                val idx = collector.tickGeneral()
                val FunctionId(retType, ps, funcST) = stackFrame.currST.lookupAll(i.s).get
                val a @ ArrayType(elemType, _, dimension) = e2.typeId.get.getType()
                val elemSize = dimension match {
                    case 1 => getTypeSize(elemType)
                    case _ => WORD_SIZE
                }
                val retSize = getTypeSize(retType)
                transExpression(e1, stackFrame)
                collector.addStatement(MoveInstr(r4, RegOp(r0)))
                transExpression(e2, stackFrame)
                collector.addStatement(
                  List(
                    MoveInstr(r3, RegOp(r0)),
                    LoadInstr(r5, r3, ImmOffset(0)),
                    AddInstr(r5, r5, ImmOffset(1)),
                    /** Load size into r0 and multiply it to get new array size */
                    LoadImmIntInstr(r0, retSize),
                    SMullInstr(r0, r5, r0, r5),
                    AddInstr(r0, r0, ImmOffset(4)),
                    PushInstr(List(r3, r4)),
                    BranchLinkInstr("malloc"),
                    PopInstr(List(r3, r4)),
                    /** Save malloc'd array for retrieval later onto the stack */
                    SubInstr(sp, sp, ImmOffset(4)),
                    StoreInstr(r0, sp, ImmOffset(0)),
                    /** Load expression array length into r1 and add 1 to it for new array length */
                    LoadInstr(r1, r3, ImmOffset(0)),
                    AddInstr(r1, r1, ImmOffset(1)),
                    /** Store new array length at its address position */
                    StoreInstr(r1, r0, ImmOffset(0)),
                    /** Increment both new and old array address by 4 to get to elements */
                    AddInstr(r3, r3, ImmOffset(4)),
                    AddInstr(r0, r0, ImmOffset(4)),
                    /** Store expression 1 into new array's first element */
                    determineStoreInstr(retType, r4, r0, 0),
                    /** Move new element to r2 for further usage */
                    MoveInstr(r2, RegOp(r0)),
                    BranchInstr(s"Scan_cond_$idx"),
                    Label(s"Scan_$idx"),
                    PushInstr(List(r1, r2, r3))
                  )
                )
                elemSize match {
                    case WORD_SIZE =>
                        collector.addStatement(
                          List(
                            LoadInstr(r0, r2, ImmOffset(0)),
                            StoreInstr(r0, sp, ImmOffset(-4), true),
                            LoadInstr(r0, r3, ImmOffset(0)),
                            StoreInstr(r0, sp, ImmOffset(-4), true)
                          )
                        )
                    case _ =>
                        collector.addStatement(
                          List(
                            LoadRegSignedByte(r0, r2, ImmOffset(0)),
                            StoreByteInstr(r0, sp, ImmOffset(-1), true),
                            LoadRegSignedByte(r0, r3, ImmOffset(0)),
                            StoreByteInstr(r0, sp, ImmOffset(-1), true)
                          )
                        )
                }
                collector.addStatement(
                  List(
                    BranchLinkInstr(s"f_${i.s}"),
                    AddInstr(sp, sp, ImmOffset(elemSize * 2)),
                    PopInstr(List(r1, r2, r3)),
                    AddInstr(r2, r2, ImmOffset(retSize)),
                    determineStoreInstr(retType, r0, r2, 0),
                    AddInstr(r3, r3, ImmOffset(elemSize)),
                    SubInstr(r1, r1, ImmOffset(1)),
                    Label(s"Scan_cond_$idx"),
                    CompareInstr(r1, ImmOffset(0)),
                    BranchInstr(s"Scan_$idx", Condition.NE),
                    LoadInstr(r0, sp, ImmOffset(0)),
                    AddInstr(sp, sp, ImmOffset(4))
                  )
                )
            }
        }
    }
}
