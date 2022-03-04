import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable.ListBuffer
import constants._
import Helpers._

class CodeGenSpec extends AnyFlatSpec {
    import parsley.{Success, Failure}
    import testUtils.{assertCodegenEquals}

    var sf: StackFrame = StackFrame(SymbolTable())
    implicit var wbuffer: WaccBuffer = new WaccBuffer
    
    /** Resets the stack frame and buffer */
    def reset(): Unit = {
        sf = StackFrame(SymbolTable())
        wbuffer = new WaccBuffer
    }

    /** Translates an expression into a list of instructions and compares it
     *  with the expected output */
    def testExpr(node: ExprNode, expected: List[Instruction]): Unit = {
        transExpression(node, sf)
        assertCodegenEquals(expected, wbuffer.emit())
    }

    def testRHS(node: AssignRHSNode, expected: List[Instruction]): Unit = {
        transRHS(node, sf)
        assertCodegenEquals(expected, wbuffer.emit())
    }

    def expectedDataSection(directives: List[List[Instruction]]): List[Instruction] = {
        val expected: ListBuffer[Instruction] = ListBuffer(Directive("data"))
        var msgNo: Int = 0
        directives.foreach(d => {
            expected += Label(s"msg_$msgNo")
            msgNo = msgNo + 1
            d.foreach(i => expected += i)
        })
        expected.toList
    }

    /** Expected directive for an overflow error in the data section */
    val expectedOverflowDirective: List[Instruction] = List(
        Directive("word 83"),
        Directive("ascii \"OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n\\0\"")
    )

    /** Expected directive for print string in the data section */
    val expectedPrintStrDirective: List[Instruction] = List(
        Directive("word 5"),
        Directive("ascii \"%.*s\\0\"")
    )

    /** Expected directive for a divide by zero error in the data section*/
    val expectedDivideByZeroDirective: List[Instruction] = List(
        Directive("word 45"),
        Directive("ascii \"DivideByZeroError: divide or modulo by zero\\n\\0\"")
    )

    val expectedNullReferenceDirective: List[Instruction] = List(
        Directive("word 50"),
        Directive("ascii \"NullReferenceError: dereference a null reference\\n\\0\"")
    )

    /** Returns a list of expected instructions in the text section */
    def expectedTextSection(
        sections: List[List[Instruction]]
    ): List[Instruction] = {
        val expected: ListBuffer[Instruction] = ListBuffer()
        sections.foreach(s => s.foreach(i => expected += i))
        expected.toList
    }

    /** Expected instruction output for overflow errors */
    def expectedOverflowText(msgNo: Int): List[Instruction] = List(
        Label("p_throw_overflow_error"),
        LoadLabelInstr(Reg(0), s"msg_$msgNo"),
        BranchLinkInstr("p_throw_runtime_error")
    )

    /** Expected instruction output for runtime errors */
    val expectedRuntimeErrText: List[Instruction] = List(
        Label("p_throw_runtime_error"),
        BranchLinkInstr("p_print_string"),
        MoveInstr(Reg(0), ImmOffset(-1)),
        BranchLinkInstr("exit")
    )

    /** Expected instruction output for print string */
    def expectedPrintStrText(msgNo: Int): List[Instruction] = List(
        Label("p_print_string"),
        PushInstr(List(LinkReg())),
        LoadInstr(Reg(1), Reg(0), ImmOffset(0)),
        AddInstr(Reg(2), Reg(0), ImmOffset(4)),
        LoadLabelInstr(Reg(0), s"msg_$msgNo"),
        AddInstr(Reg(0), Reg(0), ImmOffset(4)),
        BranchLinkInstr("printf"),
        MoveInstr(Reg(0), ImmOffset(0)),
        BranchLinkInstr("fflush"),
        PopInstr(List(PCReg()))
    )

    def expectedDivideByZeroText(msgNo: Int): List[Instruction] = List(
        Label("p_check_divide_by_zero"),
        PushInstr(List(lr)),
        CompareInstr(r1, ImmOffset(0)),
        LoadLabelInstr(r0, s"msg_$msgNo", Condition.EQ),
        BranchLinkInstr("p_throw_runtime_error", Condition.EQ),
        PopInstr(List(pc))
    )

    def expectedNullPointerText(msgNo: Int): List[Instruction] = List(
        Label("p_check_null_pointer"), 
        PushInstr(List(lr)), 
        CompareInstr(r0,ImmOffset(0), Condition.AL), 
        LoadLabelInstr(r0,s"msg_$msgNo",Condition.EQ), 
        BranchLinkInstr("p_throw_runtime_error", Condition.EQ),
        PopInstr(List(pc))
    )

    behavior of "expression code generation"
    it should "translate integer literals" in {
        reset()
        testExpr(IntLiterNode(1)(0,0), List(LoadImmIntInstr(Reg(0), 1)))
        reset()
        testExpr(IntLiterNode(-23)(0,0), List(LoadImmIntInstr(Reg(0), -23)))
    }
    it should "translate character literals" in {
        reset()
        testExpr(
            CharLiterNode('d')(0,0), 
            List(MoveInstr(Reg(0), ImmOffset('d')))
        )
    }
    it should "translate boolean literals" in {
        reset()
        testExpr(
            BoolLiterNode(true)(0,0), 
            List(MoveInstr(Reg(0), ImmOffset(1)))
        )
        reset()
        testExpr(
            BoolLiterNode(false)(0,0),
            List(MoveInstr(Reg(0), ImmOffset(0)))
        )
    }
    it should "translate pair literals" in {
        reset()
        testExpr(
            new PairLiterNode()(0,0),
            List(MoveInstr(Reg(0), ImmOffset(0)))
        )
    }
    it should "translate addition expressions" in {
        // Simple addition expression (4 + 12)
        reset()
        testExpr(
            Add(IntLiterNode(4)(0,0), IntLiterNode(12)(0,0))(0,0),

            expectedDataSection(List(
                expectedOverflowDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(Reg(0), 4),
                    PushInstr(List(Reg(0))),
                    LoadImmIntInstr(Reg(0), 12),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    AddInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                ),
                expectedOverflowText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )

        // Nested expression (2 + (3 + 4)) + (((-1) + 5) + (6 + 10))
        reset()
        testExpr(
            Add(
                Add(
                    IntLiterNode(2)(0,0), 
                    Add(IntLiterNode(3)(0,0), IntLiterNode(4)(0,0))(0,0)
                )(0,0),
                Add(
                    Add(IntLiterNode(-1)(0,0), IntLiterNode(5)(0,0))(0,0),
                    Add(IntLiterNode(6)(0,0), IntLiterNode(10)(0,0))(0,0)
                )(0,0)
            )(0,0),
            
            expectedDataSection(List(
                expectedOverflowDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(r0, 2),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 3),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 4),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    AddInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    AddInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, -1),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 5),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    AddInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 6),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 10),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    AddInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    AddInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    AddInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                ),
                expectedOverflowText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )
    }
    it should "translate subtraction expressions" in {
        // Simple expression (10 - 25)
        reset()
        testExpr(
            Sub(IntLiterNode(10)(0,0), IntLiterNode(25)(0,0))(0,0),

            expectedDataSection(List(
                expectedOverflowDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(Reg(0), 10),
                    PushInstr(List(Reg(0))),
                    LoadImmIntInstr(Reg(0), 25),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SubInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                ),
                expectedOverflowText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )

        // Nested expression (20 - (31 - 4)) - (((-10) - 5) - 6)
        reset()
        testExpr(
            Sub(
                Sub(
                    IntLiterNode(20)(0,0),
                    Sub(IntLiterNode(31)(0,0), IntLiterNode(4)(0,0))(0,0)
                )(0,0),
                Sub(
                    Sub(IntLiterNode(-10)(0,0), IntLiterNode(5)(0,0))(0,0),
                    IntLiterNode(6)(0,0)
                )(0,0)
            )(0,0),

            expectedDataSection(List(
                expectedOverflowDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(r0, 20),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 31),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 4),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SubInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SubInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, -10),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 5),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SubInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 6),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SubInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SubInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                ),
                expectedOverflowText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )
    }
    it should "translate multiplication expressions" in {
        // Simple expression (5 * 7)
        reset()
        testExpr(
            Mult(IntLiterNode(5)(0,0), IntLiterNode(7)(0,0))(0,0),

            expectedDataSection(List(
                expectedOverflowDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(r0, 5),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 7),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SMullInstr(r0, r1, r0, r1),
                    CompareInstr(r1, ASRRegOp(r0, ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE)
                ),
                expectedOverflowText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )

        // Nested expression (4 * 2) * (-5 * 10)
        reset()
        testExpr(
            Mult(
                Mult(IntLiterNode(4)(0,0), IntLiterNode(2)(0,0))(0,0),
                Mult(IntLiterNode(-5)(0,0), IntLiterNode(10)(0,0))(0,0)
            )(0,0),

            expectedDataSection(List(
                expectedOverflowDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(r0, 4),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 2),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SMullInstr(r0, r1, r0, r1),
                    CompareInstr(r1, ASRRegOp(r0, ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, -5),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 10),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SMullInstr(r0, r1, r0, r1),
                    CompareInstr(r1, ASRRegOp(r0, ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    SMullInstr(r0, r1, r0, r1),
                    CompareInstr(r1, ASRRegOp(r0, ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE)
                ),
                expectedOverflowText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )
    }
    it should "translate division expressions" in {
        // Simple expression (-4 / -9)
        reset()
        testExpr(
            Div(IntLiterNode(-4)(0,0), IntLiterNode(-9)(0,0))(0,0),
        
            expectedDataSection(List(
                expectedDivideByZeroDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(r0, -4),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, -9),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idiv")
                ),
                expectedDivideByZeroText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )

        // Nested expression (1 / (2 / (3 / (4 / 5))))
        reset()
        testExpr(
            Div(
                IntLiterNode(1)(0,0),
                Div(
                    IntLiterNode(2)(0,0),
                    Div(
                        IntLiterNode(3)(0,0),
                        Div(
                            IntLiterNode(4)(0,0),
                            IntLiterNode(5)(0,0)
                        )(0,0)
                    )(0,0)
                )(0,0)
            )(0,0),

            expectedDataSection(List(
                expectedDivideByZeroDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(r0, 1),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 2),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 3),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 4),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 5),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idiv"),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idiv"),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idiv"),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idiv")
                ),
                expectedDivideByZeroText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )
    }
    it should "translate modulus expressions" in {
        // Simple expression (230 % 9)
        reset()
        testExpr(
            Mod(IntLiterNode(230)(0,0), IntLiterNode(9)(0,0))(0,0),

            expectedDataSection(List(
                expectedDivideByZeroDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(r0, 230),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 9),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idivmod"),
                    MoveInstr(r0, RegOp(r1))
                ),
                expectedDivideByZeroText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )

        // Nested expression (10 % 9 % 8 % 7 % 6)
        reset()
        testExpr(
            Mod(
                Mod(
                    Mod(
                        Mod(
                            IntLiterNode(10)(0,0), 
                            IntLiterNode(9)(0,0)
                        )(0,0),
                        IntLiterNode(8)(0,0)
                    )(0,0),
                    IntLiterNode(7)(0,0)
                )(0,0),
                IntLiterNode(6)(0,0)                
            )(0,0),

            expectedDataSection(List(
                expectedDivideByZeroDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(r0, 10),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 9),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idivmod"),
                    MoveInstr(r0, RegOp(r1)),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 8),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idivmod"),
                    MoveInstr(r0, RegOp(r1)),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 7),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idivmod"),
                    MoveInstr(r0, RegOp(r1)),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 6),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    BranchLinkInstr("p_check_divide_by_zero"),
                    BranchLinkInstr("__aeabi_idivmod"),
                    MoveInstr(r0, RegOp(r1))
                ),
                expectedDivideByZeroText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )
    }

    it should "translate logical AND expressions" in {
        // Simple expression false && true
        reset()
        testExpr(
            And(BoolLiterNode(false)(0,0), BoolLiterNode(true)(0,0))(0,0),
            
            expectedTextSection(List(
                List(
                    MoveInstr(r0, ImmOffset(0)),
                    CompareInstr(r0, ImmOffset(0)),
                    BranchInstr("L0", Condition.EQ),
                    MoveInstr(r0, ImmOffset(1)),
                    Label("L0")
                )
            ))
        )

        // Nested expression (true && false) && false
        reset()
        testExpr(
            And(
                And(
                    BoolLiterNode(true)(0,0),
                    BoolLiterNode(false)(0,0)
                )(0,0),
                BoolLiterNode(false)(0,0)
            )(0,0),

            expectedTextSection(List(
                List(
                    MoveInstr(r0, ImmOffset(1)),
                    CompareInstr(r0, ImmOffset(0)),
                    BranchInstr("L0", Condition.EQ),
                    MoveInstr(r0, ImmOffset(0)),
                    Label("L0"),
                    CompareInstr(r0, ImmOffset(0)),
                    BranchInstr("L1", Condition.EQ),
                    MoveInstr(r0, ImmOffset(0)),
                    Label("L1")
                )
            ))
        )
    }
    it should "translate logical OR expressions" in {
        // Simple expression (true || false)
        reset()
        testExpr(
            Or(BoolLiterNode(true)(0,0), BoolLiterNode(false)(0,0))(0,0),

            expectedTextSection(List(
                List(
                    MoveInstr(r0, ImmOffset(1)),
                    CompareInstr(r0, ImmOffset(1)),
                    BranchInstr("L0", Condition.EQ),
                    MoveInstr(r0, ImmOffset(0)),
                    Label("L0")
                )
            ))
        )

        // Nested expression (false || (true || false) || true)
        reset()
        testExpr(
            Or(
                Or(
                    BoolLiterNode(false)(0,0),
                    Or(
                        BoolLiterNode(true)(0,0),
                        BoolLiterNode(false)(0,0)
                    )(0,0)
                )(0,0),
                BoolLiterNode(true)(0,0)
            )(0,0),

            expectedTextSection(List(
                List(
                    MoveInstr(r0, ImmOffset(0)),
                    CompareInstr(r0, ImmOffset(1)),
                    BranchInstr("L0", Condition.EQ),
                    MoveInstr(r0, ImmOffset(1)),
                    CompareInstr(r0, ImmOffset(1)),
                    BranchInstr("L1", Condition.EQ),
                    MoveInstr(r0, ImmOffset(0)),
                    Label("L1"),
                    Label("L0"),
                    CompareInstr(r0, ImmOffset(1)),
                    BranchInstr("L2", Condition.EQ),
                    MoveInstr(r0, ImmOffset(1)),
                    Label("L2")
                )
            ))
        )
    }
    it should "translate greater-than expressions" in {
        reset()
        testExpr(
            GT(CharLiterNode('a')(0,0), CharLiterNode('g')(0,0))(0,0),

            expectedTextSection(List(
                List(
                    MoveInstr(r0, ImmOffset('a')),
                    PushInstr(List(r0)),
                    MoveInstr(r0, ImmOffset('g')),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.GT),
                    MoveInstr(r0, ImmOffset(0), Condition.LE)
                )
            ))
        )
    }
    it should "translate greater-than-or-equal expressions" in {
        reset()
        testExpr(
            GTE(CharLiterNode('r')(0,0), CharLiterNode('q')(0,0))(0,0),

            expectedTextSection(List(
                List(
                    MoveInstr(r0, ImmOffset('r')),
                    PushInstr(List(r0)),
                    MoveInstr(r0, ImmOffset('q')),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.GE),
                    MoveInstr(r0, ImmOffset(0), Condition.LT)
                )
            ))
        )
    }

    behavior of "AssignRHS code generation"
    it should "translate expressions" in {
        reset()
        testRHS(IntLiterNode(-10)(0,0), List(LoadImmIntInstr(Reg(0), -10)))
        reset()
        testRHS(
            Add(IntLiterNode(3)(0,0), IntLiterNode(-4)(0,0))(0,0),
            expectedDataSection(List(
                expectedOverflowDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    LoadImmIntInstr(Reg(0), 3),
                    PushInstr(List(Reg(0))),
                    LoadImmIntInstr(Reg(0), -4),
                    MoveInstr(Reg(1), RegOp(Reg(0))),
                    PopInstr(List(Reg(0))),
                    AddInstr(Reg(0), Reg(0), RegOp(Reg(1)), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                ),
                expectedOverflowText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )
        reset()
        testRHS(
            new PairLiterNode()(0,0),
            List(MoveInstr(Reg(0), ImmOffset(0)))
        )
    }

    it should "translate array literals" in {
        reset()
        var node : AssignRHSNode = ArrayLiterNode(List())(0,0)
        node.typeId = Some(AnyType())    
        testRHS(
            node,
            List(
                MoveInstr(Reg(0),ImmOffset(WORD_SIZE)),
                BranchLinkInstr("malloc", Condition.AL),
                MoveInstr(Reg(3), RegOp(Reg(0))),
                MoveInstr(Reg(0), ImmOffset(0)),
                StoreInstr(Reg(0),Reg(3),ImmOffset(0)),
                MoveInstr(Reg(0), RegOp(Reg(3))) 
                )
        )
        reset()
        node = ArrayLiterNode(List(IntLiterNode(3)(0,0)))(0,0)
        node.typeId = Some(ArrayType(IntType(), List(1), 1)) 
        testRHS(
            node,
            List(
                MoveInstr(Reg(0),ImmOffset(8)),
                BranchLinkInstr("malloc", Condition.AL),
                MoveInstr(Reg(3), RegOp(Reg(0))),
                LoadImmIntInstr(Reg(0), 3),  
                StoreInstr(Reg(0), Reg(3), ImmOffset(4)),
                MoveInstr(Reg(0), ImmOffset(1)),
                StoreInstr(Reg(0),Reg(3),ImmOffset(0)),
                MoveInstr(Reg(0), RegOp(Reg(3))) 
                )
        )
        reset()
        node = ArrayLiterNode(
            List(CharLiterNode('a')(0,0), CharLiterNode('b')(0,0)))(0,0)
        node.typeId = Some(ArrayType(CharType(), List(2), 1)) 
        testRHS(
            node,
            List(
                MoveInstr(Reg(0),ImmOffset(6)),
                BranchLinkInstr("malloc", Condition.AL),
                MoveInstr(Reg(3), RegOp(Reg(0))),
                MoveInstr(Reg(0), ImmOffset('a')),  
                StoreByteInstr(Reg(0), Reg(3), ImmOffset(4)),
                MoveInstr(Reg(0), ImmOffset('b')),  
                StoreByteInstr(Reg(0), Reg(3), ImmOffset(5)),
                MoveInstr(Reg(0), ImmOffset(2)),
                StoreInstr(Reg(0),Reg(3),ImmOffset(0)),
                MoveInstr(Reg(0), RegOp(Reg(3))) 
                )
        )   
    }
    it should "translate newpair constructor" in {
        reset()
        testRHS(
            NewPairNode(IntLiterNode(1)(0,0), IntLiterNode(2)(0,0))(0,0),
            List(
                LoadImmIntInstr(Reg(0), 1),
                PushInstr(List(Reg(0))),
                MoveInstr(Reg(0), ImmOffset(4)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(Reg(1))),
                StoreInstr(Reg(1), Reg(0), ImmOffset(0)),
                PushInstr(List(Reg(0))),
                LoadImmIntInstr(Reg(0), 2),
                PushInstr(List(Reg(0))),
                MoveInstr(Reg(0), ImmOffset(4)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(Reg(1))),
                StoreInstr(Reg(1), Reg(0), ImmOffset(0)),
                PushInstr(List(Reg(0))),
                MoveInstr(Reg(0), ImmOffset(8)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(Reg(1), Reg(2))),
                StoreInstr(Reg(2), Reg(0), ImmOffset(0), false),
                StoreInstr(Reg(1), Reg(0), ImmOffset(4), false)

            )
        )
        reset()
        testRHS(
            NewPairNode(CharLiterNode('z')(0,0), BoolLiterNode(true)(0,0))(0,0),
            List(
                MoveInstr(Reg(0), ImmOffset('z')),
                PushInstr(List(Reg(0))),
                MoveInstr(Reg(0), ImmOffset(1)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(Reg(1))),
                StoreByteInstr(Reg(1), Reg(0), ImmOffset(0)),
                PushInstr(List(Reg(0))),
                MoveInstr(Reg(0), ImmOffset(1)),
                PushInstr(List(Reg(0))),
                MoveInstr(Reg(0), ImmOffset(1)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(Reg(1))),
                StoreByteInstr(Reg(1), Reg(0), ImmOffset(0)),
                PushInstr(List(Reg(0))),
                MoveInstr(Reg(0), ImmOffset(8)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(Reg(1), Reg(2))),
                StoreInstr(Reg(2), Reg(0), ImmOffset(0), false),
                StoreInstr(Reg(1), Reg(0), ImmOffset(4), false)

            )
        )
    }
    it should "translate pair elements" in {
        reset()
        var node : PairElemNode = FirstPairElemNode(IdentNode("x")(0,0))(0,0)
        node.typeId = Some(IntType()) 
        var st = SymbolTable()
        st.add("x", PairType(IntType(), CharType()))
        sf = StackFrame(st)
		sf.unlock("x")

        testRHS(
            node,
            expectedDataSection(List(
                expectedNullReferenceDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    BranchLinkInstr("p_check_null_pointer"),
                    LoadInstr(r0, sp, ImmOffset(0)),
                    LoadInstr(Reg(0), Reg(0),ImmOffset(0)), 
                    LoadInstr(Reg(0), Reg(0),ImmOffset(0))
                ),
                expectedNullPointerText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )
        reset()
        node = SecondPairElemNode(IdentNode("x")(0,0))(0,0)
        node.typeId = Some(CharType()) 
        st = SymbolTable()
        st.add("x", PairType(IntType(), CharType()))
        sf = StackFrame(st)
        testRHS(
            node,
            expectedDataSection(List(
                expectedNullReferenceDirective,
                expectedPrintStrDirective
            )) ++
            expectedTextSection(List(
                List(
                    BranchLinkInstr("p_check_null_pointer"),
                    LoadInstr(r0, sp, ImmOffset(0)),
                    LoadInstr(Reg(0), Reg(0),ImmOffset(4)), 
                    LoadRegSignedByte(r0,r0,ImmOffset(0),Condition.AL)
                ),
                expectedNullPointerText(0),
                expectedRuntimeErrText,
                expectedPrintStrText(1)
            ))
        )
    }
    it should "translate call statements" in {}

}
