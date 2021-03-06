import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable.ListBuffer
import constants._
import Helpers._
import parsley.internal.machine.instructions.Pop
import parsley.internal.machine.instructions.Pop

class CodeGenSpec extends AnyFlatSpec {
    import parsley.{Success, Failure}
    import testUtils.{assertCodegenEquals}

    implicit val repr: Representation = ARMRepresentation
    var sf: StackFrame = StackFrame(SymbolTable())
    implicit var wbuffer: WaccBuffer = new WaccBuffer

    /** Resets the stack frame and buffer */
    def reset(): Unit = {
        sf = StackFrame(SymbolTable())
        wbuffer = new WaccBuffer
    }

    /** Translates an expression into a list of instructions and compares it
      * with the expected output
      */
    def testExpr(node: ExprNode, expected: List[Instruction]): Unit = {
        transExpression(node, sf)
        assertCodegenEquals(expected, wbuffer.emit())
    }

    def testRHS(node: AssignRHSNode, expected: List[Instruction]): Unit = {
        transRHS(node, sf)
        assertCodegenEquals(expected, wbuffer.emit())
    }

    def testStat(node: StatNode, expected: List[Instruction]): Unit = {
        transStatement(node, sf)
        assertCodegenEquals(expected, wbuffer.emit())
    }

    def testFunction(node: FuncNode, expected: List[Instruction]): Unit = {
        transFunction(node, sf)
        assertCodegenEquals(expected, wbuffer.emit())
    }

    def testLHS(node: AssignLHSNode, expected: List[Instruction]): Unit = {
        transLHS(node, sf)
        assertCodegenEquals(expected, wbuffer.emit())
    }

    def expectedDataSection(
        directives: List[List[Instruction]]
    ): List[Instruction] = {
        val expected: ListBuffer[Instruction] = ListBuffer(Directive("data"))
        var msgNo: Int = 0
        directives.foreach(d => {
            expected += Label(s"msg_$msgNo")
            msgNo = msgNo + 1
            d.foreach(i => expected += i)
        })
        expected.toList
    }

    def expectedBssSection = List(
      Directive("bss"),
      Label("catch_address"),
      Directive("skip 4"),
      Label("prev_sp"),
      Directive("skip 4")
    )

    /** Expected directive for an overflow error in the data section */
    val expectedOverflowDirective: List[Instruction] = List(
      Directive("word 83"),
      Directive(
        "ascii \"OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n\\0\""
      )
    )

    /** Expected directive for print string in the data section */
    val expectedPrintStrDirective: List[Instruction] = List(
      Directive("word 5"),
      Directive("ascii \"%.*s\\0\"")
    )

    val expectedPrintIntDirective: List[Instruction] = List(
      Directive(s"word 3"),
      Directive(s"ascii \"%d\\0\"")
    )

    val expectedPrintCharDirective: List[Instruction] = List(
      Directive(s"word 4"),
      Directive(s"ascii \" %c\\0\"")
    )

    val expectedPrintRefDirective: List[Instruction] = List(
      Directive(s"word 3"),
      Directive(s"ascii \"%p\\0\"")
    )

    val expectedPrintLnDirective: List[Instruction] = List(
      Directive(s"word 1"),
      Directive(s"ascii \"\\0\"")
    )

    /** Expected directive for a divide by zero error in the data section */
    val expectedDivideByZeroDirective: List[Instruction] = List(
      Directive("word 45"),
      Directive("ascii \"DivideByZeroError: divide or modulo by zero\\n\\0\"")
    )

    /** Expected directive for a null reference error in the data section */
    val expectedNullReferenceDirective: List[Instruction] = List(
      Directive("word 50"),
      Directive(
        "ascii \"NullReferenceError: dereference a null reference\\n\\0\""
      )
    )

    val expectedArrayNegIndexDirective: List[Instruction] = List(
      Directive("word 44"),
      Directive("ascii \"ArrayIndexOutOfBoundsError: negative index\\n\\0\"")
    )

    val expectedArrayIndexTooLargeDirective: List[Instruction] = List(
      Directive("word 45"),
      Directive("ascii \"ArrayIndexOutOfBoundsError: index too large\\n\\0\"")
    )

    /** Expected directive for a string in the data section */
    def expectedStringDirective(str: String): List[Instruction] = List(
      Directive(s"word ${str.length()}"),
      Directive(s"ascii \"$str\"")
    )

    /** Expected directive for an integer in the data section */
    def expectedIntDirective: List[Instruction] = List(
      Directive("word 3"),
      Directive("ascii \"%d\\0\"")
    )

    /** Expected directive for a char in the data section */
    def expectedCharDirective: List[Instruction] = List(
      Directive("word 4"),
      Directive("ascii \" %c\\0\"")
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
      LoadLabelInstr(r0, s"msg_$msgNo"),
      BranchLinkInstr("p_throw_runtime_error")
    )

    /** Expected instruction output for runtime errors */
    val expectedRuntimeErrText: List[Instruction] = List(
      Label("p_throw_runtime_error"),
      LoadImmIntInstr(r7, 5),
      LoadLabelInstr(r2, "catch_address"),
      LoadInstr(r2, r2, ImmOffset(0)),
      CompareInstr(r2, ImmOffset(0)),
      MoveInstr(pc, RegOp(r2), Condition.NE),
      BranchLinkInstr("p_print_string"),
      MoveInstr(r0, ImmOffset(-1)),
      BranchLinkInstr("exit")
    )

    /** Expected instruction output for print string */
    def expectedPrintStrText(msgNo: Int): List[Instruction] = List(
      Label("p_print_string"),
      PushInstr(List(lr)),
      LoadInstr(r1, r0, ImmOffset(0)),
      AddInstr(r2, r0, ImmOffset(4)),
      LoadLabelInstr(r0, s"msg_$msgNo"),
      AddInstr(r0, r0, ImmOffset(4)),
      BranchLinkInstr("printf"),
      MoveInstr(r0, ImmOffset(0)),
      BranchLinkInstr("fflush"),
      PopInstr(List(pc))
    )

    def expectedFreePairText(msgNo: Int): List[Instruction] = {
        List(
          Label("p_free_pair"),
          PushInstr(List(lr)),
          CompareInstr(r0, ImmOffset(0)),
          LoadLabelInstr(r0, s"msg_$msgNo", Condition.EQ),
          BranchInstr("p_throw_runtime_error", Condition.EQ),
          PushInstr(List(r0)),
          LoadInstr(r0, r0, ImmOffset(0)),
          BranchLinkInstr("free"),
          LoadInstr(r0, sp, ImmOffset(0)),
          LoadInstr(r0, r0, ImmOffset(4)),
          BranchLinkInstr("free"),
          PopInstr(List(r0)),
          BranchLinkInstr("free"),
          PopInstr(List(pc))
        )
    }
    def expectedPrintIntText(msgNo: Int): List[Instruction] = List(
      Label("p_print_int"),
      PushInstr(List(lr)),
      MoveInstr(r1, RegOp(r0)),
      LoadLabelInstr(r0, s"msg_$msgNo"),
      AddInstr(r0, r0, ImmOffset(4), false),
      BranchLinkInstr("printf"),
      MoveInstr(r0, ImmOffset(0)),
      BranchLinkInstr("fflush"),
      PopInstr(List(pc))
    )

    def expectedPrintRefText(msgNo: Int): List[Instruction] =
        List(
          Label("p_print_reference"),
          PushInstr(List(lr)),
          MoveInstr(r1, RegOp(r0)),
          LoadLabelInstr(r0, s"msg_$msgNo"),
          AddInstr(r0, r0, ImmOffset(4), false),
          BranchLinkInstr("printf"),
          MoveInstr(r0, ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(List(pc))
        )

    def expectedPrintLnText(msgNo: Int): List[Instruction] = {
        List(
          Label("p_print_ln"),
          PushInstr(List(lr)),
          LoadLabelInstr(r0, s"msg_$msgNo"),
          AddInstr(r0, r0, ImmOffset(4), false),
          BranchLinkInstr("puts"),
          MoveInstr(r0, ImmOffset(0)),
          BranchLinkInstr("fflush"),
          PopInstr(List(pc))
        )
    }

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
      CompareInstr(r0, ImmOffset(0), Condition.AL),
      LoadLabelInstr(r0, s"msg_$msgNo", Condition.EQ),
      BranchLinkInstr("p_throw_runtime_error", Condition.EQ),
      PopInstr(List(pc))
    )

    def expectedCheckArrayBoundsText(msgNo: Int): List[Instruction] = {
        val next_msg: Int = msgNo + 1
        List(
          Label("p_check_array_bounds"),
          PushInstr(List(lr)),
          CompareInstr(r0, ImmOffset(0), Condition.AL),
          LoadLabelInstr(r0, s"msg_$msgNo", Condition.LT),
          BranchLinkInstr("p_throw_runtime_error", Condition.LT),
          LoadInstr(r1, r4, ImmOffset(0), Condition.AL),
          CompareInstr(r0, RegOp(r1), Condition.AL),
          LoadLabelInstr(r0, s"msg_$next_msg", Condition.CS),
          BranchLinkInstr("p_throw_runtime_error", Condition.CS),
          PopInstr(List(pc))
        )
    }

    behavior of "expression code generation"
    it should "translate integer literals" in {
        reset()
        testExpr(
          IntLiterNode(1)(0, 0),
          expectedBssSection ++: List(LoadImmIntInstr(r0, 1))
        )
        reset()
        testExpr(
          IntLiterNode(-23)(0, 0),
          expectedBssSection ++: List(LoadImmIntInstr(r0, -23))
        )
    }
    it should "translate character literals" in {
        reset()
        testExpr(
          CharLiterNode('d')(0, 0),
          expectedBssSection ++: List(MoveInstr(r0, ImmOffset('d')))
        )
    }
    it should "translate boolean literals" in {
        reset()
        testExpr(
          BoolLiterNode(true)(0, 0),
          expectedBssSection ++: List(MoveInstr(r0, ImmOffset(1)))
        )
        reset()
        testExpr(
          BoolLiterNode(false)(0, 0),
          expectedBssSection ++: List(MoveInstr(r0, ImmOffset(0)))
        )
    }
    it should "translate pair literals" in {
        reset()
        testExpr(
          new PairLiterNode()(0, 0),
          expectedBssSection ++: List(MoveInstr(r0, ImmOffset(0)))
        )
    }
    it should "translate string literals" in {
        reset()
        testExpr(
          StringLiterNode("a string.")(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedStringDirective("a string.")
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadLabelInstr(r0, "msg_0")
                  )
                )
              )
        )
    }
    it should "translate logical NOT expressions" in {
        reset()
        testExpr(
          Not(BoolLiterNode(false)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    MoveInstr(r0, ImmOffset(0)),
                    XorInstr(r0, r0, ImmOffset(1))
                  )
                )
              )
        )
    }
    it should "translate negation expressions" in {
        reset()
        testExpr(
          Neg(IntLiterNode(53)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedOverflowDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadImmIntInstr(r0, 53),
                    ReverseSubInstr(r0, r0, ImmOffset(0), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                  ),
                  expectedOverflowText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )
    }
    it should "translate len expressions" in {
        reset()
        val st = SymbolTable()
        // var node: AssignRHSNode = ArrayLiterNode(List())(0, 0)
        // node.typeId = Some(AnyType())
        var node: IdentNode = IdentNode("anArray")(0, 0)
        node.typeId = Some(ArrayType(IntType(), List(3), 1))
        st.add(
          "anArray",
          Variable(ArrayType(IntType(), List(3), 1))
        )
        sf = StackFrame(st)
        sf.unlock("anArray")
        testExpr(
          Len(node)(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    LoadInstr(r0, sp, ImmOffset(sf.getOffset("anArray"))),
                    LoadInstr(r0, r0, ImmOffset(0))
                  )
                )
              )
        )
    }
    it should "translate ord expressions" in {
        reset()
        testExpr(
          Ord(CharLiterNode('t')(0, 0))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    MoveInstr(r0, ImmOffset('t'))
                  )
                )
              )
        )
    }
    it should "translate chr expressions" in {
        reset()
        testExpr(
          Chr(IntLiterNode(90)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    LoadImmIntInstr(r0, 90)
                  )
                )
              )
        )
    }
    it should "translate addition expressions" in {
        // Simple addition expression (4 + 12)
        reset()
        testExpr(
          Add(IntLiterNode(4)(0, 0), IntLiterNode(12)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedOverflowDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadImmIntInstr(r0, 4),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 12),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    AddInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                  ),
                  expectedOverflowText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )

        // Nested expression (2 + (3 + 4)) + (((-1) + 5) + (6 + 10))
        reset()
        testExpr(
          Add(
            Add(
              IntLiterNode(2)(0, 0),
              Add(IntLiterNode(3)(0, 0), IntLiterNode(4)(0, 0))(0, 0)
            )(0, 0),
            Add(
              Add(IntLiterNode(-1)(0, 0), IntLiterNode(5)(0, 0))(0, 0),
              Add(IntLiterNode(6)(0, 0), IntLiterNode(10)(0, 0))(0, 0)
            )(0, 0)
          )(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedOverflowDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
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
                )
              )
        )
    }
    it should "translate subtraction expressions" in {
        // Simple expression (10 - 25)
        reset()
        testExpr(
          Sub(IntLiterNode(10)(0, 0), IntLiterNode(25)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedOverflowDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadImmIntInstr(r0, 10),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 25),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SubInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                  ),
                  expectedOverflowText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )

        // Nested expression (20 - (31 - 4)) - (((-10) - 5) - 6)
        reset()
        testExpr(
          Sub(
            Sub(
              IntLiterNode(20)(0, 0),
              Sub(IntLiterNode(31)(0, 0), IntLiterNode(4)(0, 0))(0, 0)
            )(0, 0),
            Sub(
              Sub(IntLiterNode(-10)(0, 0), IntLiterNode(5)(0, 0))(0, 0),
              IntLiterNode(6)(0, 0)
            )(0, 0)
          )(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedOverflowDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadImmIntInstr(r0, 20),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 31),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 4),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SubInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SubInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, -10),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 5),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SubInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 6),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SubInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SubInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                  ),
                  expectedOverflowText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )
    }
    it should "translate multiplication expressions" in {
        // Simple expression (5 * 7)
        reset()
        testExpr(
          Mult(IntLiterNode(5)(0, 0), IntLiterNode(7)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedOverflowDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadImmIntInstr(r0, 5),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 7),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SMullInstr(r0, r1, r0, r1),
                    CompareInstr(r1, ASRRegOp(r0, ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE)
                  ),
                  expectedOverflowText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )

        // Nested expression (4 * 2) * (-5 * 10)
        reset()
        testExpr(
          Mult(
            Mult(IntLiterNode(4)(0, 0), IntLiterNode(2)(0, 0))(0, 0),
            Mult(IntLiterNode(-5)(0, 0), IntLiterNode(10)(0, 0))(0, 0)
          )(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedOverflowDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadImmIntInstr(r0, 4),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 2),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SMullInstr(r0, r1, r0, r1),
                    CompareInstr(r1, ASRRegOp(r0, ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, -5),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 10),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SMullInstr(r0, r1, r0, r1),
                    CompareInstr(r1, ASRRegOp(r0, ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    SMullInstr(r0, r1, r0, r1),
                    CompareInstr(r1, ASRRegOp(r0, ShiftImm(31))),
                    BranchLinkInstr("p_throw_overflow_error", Condition.NE)
                  ),
                  expectedOverflowText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )
    }
    it should "translate division expressions" in {
        // Simple expression (-4 / -9)
        reset()
        testExpr(
          Div(IntLiterNode(-4)(0, 0), IntLiterNode(-9)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedDivideByZeroDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
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
                )
              )
        )

        // Nested expression (1 / (2 / (3 / (4 / 5))))
        reset()
        testExpr(
          Div(
            IntLiterNode(1)(0, 0),
            Div(
              IntLiterNode(2)(0, 0),
              Div(
                IntLiterNode(3)(0, 0),
                Div(
                  IntLiterNode(4)(0, 0),
                  IntLiterNode(5)(0, 0)
                )(0, 0)
              )(0, 0)
            )(0, 0)
          )(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedDivideByZeroDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
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
                )
              )
        )
    }
    it should "translate modulus expressions" in {
        // Simple expression (230 % 9)
        reset()
        testExpr(
          Mod(IntLiterNode(230)(0, 0), IntLiterNode(9)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedDivideByZeroDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
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
                )
              )
        )

        // Nested expression (10 % 9 % 8 % 7 % 6)
        reset()
        testExpr(
          Mod(
            Mod(
              Mod(
                Mod(
                  IntLiterNode(10)(0, 0),
                  IntLiterNode(9)(0, 0)
                )(0, 0),
                IntLiterNode(8)(0, 0)
              )(0, 0),
              IntLiterNode(7)(0, 0)
            )(0, 0),
            IntLiterNode(6)(0, 0)
          )(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedDivideByZeroDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
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
                )
              )
        )
    }

    it should "translate logical AND expressions" in {
        // Simple expression false && true
        reset()
        testExpr(
          And(BoolLiterNode(false)(0, 0), BoolLiterNode(true)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    MoveInstr(r0, ImmOffset(0)),
                    CompareInstr(r0, ImmOffset(0)),
                    BranchInstr("L0", Condition.EQ),
                    MoveInstr(r0, ImmOffset(1)),
                    Label("L0")
                  )
                )
              )
        )

        // Nested expression (true && false) && false
        reset()
        testExpr(
          And(
            And(
              BoolLiterNode(true)(0, 0),
              BoolLiterNode(false)(0, 0)
            )(0, 0),
            BoolLiterNode(false)(0, 0)
          )(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
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
                )
              )
        )
    }
    it should "translate logical OR expressions" in {
        // Simple expression (true || false)
        reset()
        testExpr(
          Or(BoolLiterNode(true)(0, 0), BoolLiterNode(false)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    MoveInstr(r0, ImmOffset(1)),
                    CompareInstr(r0, ImmOffset(1)),
                    BranchInstr("L0", Condition.EQ),
                    MoveInstr(r0, ImmOffset(0)),
                    Label("L0")
                  )
                )
              )
        )

        // Nested expression (false || (true || false) || true)
        reset()
        testExpr(
          Or(
            Or(
              BoolLiterNode(false)(0, 0),
              Or(
                BoolLiterNode(true)(0, 0),
                BoolLiterNode(false)(0, 0)
              )(0, 0)
            )(0, 0),
            BoolLiterNode(true)(0, 0)
          )(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
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
                )
              )
        )
    }
    it should "translate greater-than expressions" in {
        reset()
        testExpr(
          GT(CharLiterNode('a')(0, 0), CharLiterNode('g')(0, 0))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
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
                )
              )
        )
    }
    it should "translate greater-than-or-equal expressions" in {
        reset()
        testExpr(
          GTE(CharLiterNode('r')(0, 0), CharLiterNode('q')(0, 0))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
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
                )
              )
        )
    }
    it should "translate less-than expressions" in {
        reset()
        testExpr(
          LT(IntLiterNode(50)(0, 0), IntLiterNode(4)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    LoadImmIntInstr(r0, 50),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, 4),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.LT),
                    MoveInstr(r0, ImmOffset(0), Condition.GE)
                  )
                )
              )
        )
    }
    it should "translate less-than-or-equal expressions" in {
        reset()
        testExpr(
          LTE(CharLiterNode('t')(0, 0), CharLiterNode('p')(0, 0))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    MoveInstr(r0, ImmOffset('t')),
                    PushInstr(List(r0)),
                    MoveInstr(r0, ImmOffset('p')),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.LE),
                    MoveInstr(r0, ImmOffset(0), Condition.GT)
                  )
                )
              )
        )
    }
    it should "translate equality expressions" in {
        reset()
        testExpr(
          Equal(BoolLiterNode(true)(0, 0), BoolLiterNode(false)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    MoveInstr(r0, ImmOffset(1)),
                    PushInstr(List(r0)),
                    MoveInstr(r0, ImmOffset(0)),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.EQ),
                    MoveInstr(r0, ImmOffset(0), Condition.NE)
                  )
                )
              )
        )
    }
    it should "translate inequality expressions" in {
        // String literal
        reset()
        testExpr(
          NotEqual(
            StringLiterNode("hello")(0, 0),
            StringLiterNode("goodbye")(0, 0)
          )(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedStringDirective("hello"),
                  expectedStringDirective("goodbye")
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadLabelInstr(r0, "msg_0"),
                    PushInstr(List(r0)),
                    LoadLabelInstr(r0, "msg_1"),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    CompareInstr(r0, RegOp(r1)),
                    MoveInstr(r0, ImmOffset(1), Condition.NE),
                    MoveInstr(r0, ImmOffset(0), Condition.EQ)
                  )
                )
              )
        )
    }

    behavior of "AssignRHS code generation"
    it should "translate expressions" in {
        reset()
        testRHS(
          IntLiterNode(-10)(0, 0),
          expectedBssSection ++: List(LoadImmIntInstr(r0, -10))
        )
        reset()
        testRHS(
          Add(IntLiterNode(3)(0, 0), IntLiterNode(-4)(0, 0))(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedOverflowDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadImmIntInstr(r0, 3),
                    PushInstr(List(r0)),
                    LoadImmIntInstr(r0, -4),
                    MoveInstr(r1, RegOp(r0)),
                    PopInstr(List(r0)),
                    AddInstr(r0, r0, RegOp(r1), true),
                    BranchLinkInstr("p_throw_overflow_error", Condition.VS)
                  ),
                  expectedOverflowText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )
        reset()
        testRHS(
          new PairLiterNode()(0, 0),
          expectedBssSection ++: List(MoveInstr(r0, ImmOffset(0)))
        )
    }

    it should "translate array literals" in {
        reset()
        var node: AssignRHSNode = ArrayLiterNode(List())(0, 0)
        node.typeId = Some(AnyType())
        testRHS(
          node,
          expectedBssSection ++:
              List(
                MoveInstr(r0, ImmOffset(WORD_SIZE)),
                BranchLinkInstr("malloc", Condition.AL),
                MoveInstr(r3, RegOp(r0)),
                MoveInstr(r0, ImmOffset(0)),
                StoreInstr(r0, r3, ImmOffset(0)),
                MoveInstr(r0, RegOp(r3))
              )
        )
        reset()
        node = ArrayLiterNode(List(IntLiterNode(3)(0, 0)))(0, 0)
        node.typeId = Some(ArrayType(IntType(), List(1), 1))
        testRHS(
          node,
          expectedBssSection ++:
              List(
                MoveInstr(r0, ImmOffset(8)),
                BranchLinkInstr("malloc", Condition.AL),
                MoveInstr(r3, RegOp(r0)),
                LoadImmIntInstr(r0, 3),
                StoreInstr(r0, r3, ImmOffset(4)),
                MoveInstr(r0, ImmOffset(1)),
                StoreInstr(r0, r3, ImmOffset(0)),
                MoveInstr(r0, RegOp(r3))
              )
        )
        reset()
        node = ArrayLiterNode(
          List(CharLiterNode('a')(0, 0), CharLiterNode('b')(0, 0))
        )(0, 0)
        node.typeId = Some(ArrayType(CharType(), List(2), 1))
        testRHS(
          node,
          expectedBssSection ++:
              List(
                MoveInstr(r0, ImmOffset(6)),
                BranchLinkInstr("malloc", Condition.AL),
                MoveInstr(r3, RegOp(r0)),
                MoveInstr(r0, ImmOffset('a')),
                StoreByteInstr(r0, r3, ImmOffset(4)),
                MoveInstr(r0, ImmOffset('b')),
                StoreByteInstr(r0, r3, ImmOffset(5)),
                MoveInstr(r0, ImmOffset(2)),
                StoreInstr(r0, r3, ImmOffset(0)),
                MoveInstr(r0, RegOp(r3))
              )
        )
    }
    it should "translate newpair constructor" in {
        reset()
        testRHS(
          NewPairNode(IntLiterNode(1)(0, 0), IntLiterNode(2)(0, 0))(0, 0),
          expectedBssSection ++:
              List(
                LoadImmIntInstr(r0, 1),
                PushInstr(List(r0)),
                MoveInstr(r0, ImmOffset(4)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(r1)),
                StoreInstr(r1, r0, ImmOffset(0)),
                PushInstr(List(r0)),
                LoadImmIntInstr(r0, 2),
                PushInstr(List(r0)),
                MoveInstr(r0, ImmOffset(4)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(r1)),
                StoreInstr(r1, r0, ImmOffset(0)),
                PushInstr(List(r0)),
                MoveInstr(r0, ImmOffset(8)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(r1, r2)),
                StoreInstr(r2, r0, ImmOffset(0), false),
                StoreInstr(r1, r0, ImmOffset(4), false)
              )
        )
        reset()
        testRHS(
          NewPairNode(CharLiterNode('z')(0, 0), BoolLiterNode(true)(0, 0))(
            0,
            0
          ),
          expectedBssSection ++:
              List(
                MoveInstr(r0, ImmOffset('z')),
                PushInstr(List(r0)),
                MoveInstr(r0, ImmOffset(1)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(r1)),
                StoreByteInstr(r1, r0, ImmOffset(0)),
                PushInstr(List(r0)),
                MoveInstr(r0, ImmOffset(1)),
                PushInstr(List(r0)),
                MoveInstr(r0, ImmOffset(1)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(r1)),
                StoreByteInstr(r1, r0, ImmOffset(0)),
                PushInstr(List(r0)),
                MoveInstr(r0, ImmOffset(8)),
                BranchLinkInstr("malloc", Condition.AL),
                PopInstr(List(r1, r2)),
                StoreInstr(r2, r0, ImmOffset(0), false),
                StoreInstr(r1, r0, ImmOffset(4), false)
              )
        )
    }
    it should "translate pair elements" in {
        reset()
        var node: PairElemNode = FirstPairElemNode(IdentNode("x")(0, 0))(0, 0)
        node.typeId = Some(IntType())
        var st = SymbolTable()
        st.add("x", PairType(IntType(), CharType()))
        sf = StackFrame(st)
        sf.unlock("x")

        testRHS(
          node,
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedNullReferenceDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadInstr(r0, sp, ImmOffset(0)),
                    BranchLinkInstr("p_check_null_pointer"),
                    LoadInstr(r0, r0, ImmOffset(0)),
                    LoadInstr(r0, r0, ImmOffset(0))
                  ),
                  expectedNullPointerText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )
        reset()
        node = SecondPairElemNode(IdentNode("x")(0, 0))(0, 0)
        node.typeId = Some(CharType())
        st = SymbolTable()
        st.add("x", PairType(IntType(), CharType()))
        sf = StackFrame(st)
        sf.unlock("x")
        testRHS(
          node,
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedNullReferenceDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadInstr(r0, sp, ImmOffset(0)),
                    BranchLinkInstr("p_check_null_pointer"),
                    LoadInstr(r0, r0, ImmOffset(4)),
                    LoadRegSignedByte(r0, r0, ImmOffset(0), Condition.AL)
                  ),
                  expectedNullPointerText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )
    }
    it should "translate call statements" in {
        reset()
        var node = CallNode(IdentNode("f")(0, 0), List())(0, 0)
        node.typeId = Some(CharType())
        var st = SymbolTable()
        st.add("f", FunctionId(IntType(), Array(), st))
        sf = StackFrame(st)
        sf.unlock("f")
        testRHS(
          node,
          expectedBssSection ++:
              List(BranchLinkInstr("f_f", Condition.AL))
        )

        reset()
        node = CallNode(
          IdentNode("f")(0, 0),
          List(IdentNode("x")(0, 0), IntLiterNode(1)(0, 0))
        )(0, 0)
        node.typeId = Some(BoolType())
        st = SymbolTable()
        st.add("x", BoolType())
        st.add(
          "f",
          FunctionId(IntType(), Array(Param(BoolType()), Param(IntType())), st)
        )
        sf = StackFrame(st)
        sf.unlock("x")
        sf.unlock("f")
        testRHS(
          node,
          expectedBssSection ++:
              List(
                LoadImmIntInstr(r0, 1, Condition.AL),
                StoreInstr(r0, sp, ImmOffset(-4), true),
                LoadRegSignedByte(r0, sp, ImmOffset(4), Condition.AL),
                StoreByteInstr(r0, sp, ImmOffset(-1), true),
                BranchLinkInstr("f_f", Condition.AL),
                AddInstr(sp, sp, ImmOffset(5), false)
              )
        )

        reset()
        node =
            CallNode(IdentNode("f")(0, 0), List(CharLiterNode('a')(0, 0)))(0, 0)
        node.typeId = Some(CharType())
        st = SymbolTable()
        st.add("f", FunctionId(IntType(), Array(Param(CharType())), st))
        sf = StackFrame(st)
        sf.unlock("f")
        testRHS(
          node,
          expectedBssSection ++:
              List(
                MoveInstr(r0, ImmOffset('a'), Condition.AL),
                StoreByteInstr(r0, sp, ImmOffset(-1), true),
                BranchLinkInstr("f_f", Condition.AL),
                AddInstr(sp, sp, ImmOffset(1), false)
              )
        )

    }

    it should "translate print int statements" in {
        reset()

        var node =
            StatListNode(List(PrintNode(IntLiterNode(1)(0, 0))(0, 0)))(0, 0)
        testStat(
          node,
          expectedBssSection ++:
              expectedDataSection(List(expectedPrintIntDirective))
              ++: List(
                LoadImmIntInstr(r0, 1),
                BranchLinkInstr("p_print_int")
              )
              ++: expectedPrintIntText(0)
        )
    }
    it should "translate print string statements" in {
        reset()
        var node =
            StatListNode(List(PrintNode(IntLiterNode(1)(0, 0))(0, 0)))(0, 0)
        testStat(
          node,
          expectedBssSection ++:
              expectedDataSection(List(expectedPrintIntDirective))
              ++: List(
                LoadImmIntInstr(r0, 1),
                BranchLinkInstr("p_print_int")
              )
              ++: expectedPrintIntText(0)
        )
    }
    it should "translate print char statements" in {
        reset()
        var node =
            StatListNode(List(PrintNode(CharLiterNode('c')(0, 0))(0, 0)))(0, 0)
        testStat(
          node,
          expectedBssSection ++:
              List(
                MoveInstr(r0, ImmOffset('c')),
                BranchLinkInstr("putchar")
              )
        )
    }

    behavior of "Statements code generation"
    it should "translate skip statements" in {
        reset()
        var node: StatListNode = StatListNode(List(SkipNode()(0, 0)))(0, 0)
        testStat(
          node,
          expectedBssSection
        )
    }

    it should "translate new assignment statements" in {
        reset()
        var node: StatListNode = StatListNode(
          List(
            NewAssignNode(
              IntTypeNode()(0, 0),
              IdentNode("x")(0, 0),
              IntLiterNode(0)(0, 0)
            )(0, 0)
          )
        )(0, 0)

        var st = SymbolTable()
        st.add("x", IntType())
        sf = StackFrame(st)
        sf.unlock("x")

        testStat(
          node,
          expectedBssSection ++:
              List(
                LoadImmIntInstr(r0, 0),
                StoreInstr(r0, sp, ImmOffset(0), false)
              )
        )

    }

    it should "translate free statements" in {
        // Free pair
        reset()
        val pairST = SymbolTable()
        val pair = IdentNode("a_pair")(0, 0)
        pairST.add("a_pair", Variable(PairType(IntType(), IntType())))
        pair.check(pairST, ListBuffer())
        sf = StackFrame(pairST)
        sf.unlock("a_pair")
        testStat(
          StatListNode(List(FreeNode(pair)(0, 0)))(0, 0),
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedNullReferenceDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    LoadInstr(r0, sp, ImmOffset(sf.getOffset("a_pair"))),
                    BranchLinkInstr("p_free_pair")
                  ),
                  expectedFreePairText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )

        // Free array
        reset()
        val arrST = SymbolTable()
        val node = IdentNode("arr1")(0, 0)
        arrST.add("arr1", Variable(ArrayType(IntType(), List(5), 1)))
        node.check(arrST, ListBuffer())
        sf = StackFrame(arrST)
        sf.unlock("arr1")
        testStat(
          StatListNode(List(FreeNode(node)(0, 0)))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    LoadInstr(r0, sp, ImmOffset(sf.getOffset("arr1"))),
                    BranchLinkInstr("free")
                  )
                )
              )
        )
    }

    it should "translate exit statements" in {
        reset()
        testStat(
          StatListNode(List(ExitNode(IntLiterNode(5)(0, 0))(0, 0)))(0, 0),
          expectedBssSection ++:
              expectedTextSection(
                List(
                  List(
                    LoadImmIntInstr(r0, 5),
                    BranchLinkInstr("exit")
                  )
                )
              )
        )
    }

    it should "translate read (int) statements" in {
        reset()
        var identifier = IdentNode("x")(0, 0)
        identifier.typeId = Some(IntType())
        var node: StatListNode =
            StatListNode(List(ReadNode(identifier)(0, 0)))(0, 0)
        var st = SymbolTable()
        st.add("x", IntType())
        sf = StackFrame(st)
        sf.unlock("x")

        val readIntInstructions: List[Instruction] = List(
          Label("p_read_int"),
          PushInstr(List(lr)),
          MoveInstr(r1, RegOp(r0)),
          LoadLabelInstr(r0, "msg_0"),
          AddInstr(r0, r0, ImmOffset(4)),
          BranchLinkInstr("scanf"),
          PopInstr(List(pc))
        )

        testStat(
          node,
          expectedBssSection ++:
              expectedDataSection(
                List(expectedIntDirective)
              ) ++: expectedTextSection(
                List(
                  List(
                    AddInstr(r0, sp, ImmOffset(0)),
                    BranchLinkInstr("p_read_int")
                  ) ++: readIntInstructions
                )
              )
        )
    }

    it should "translate read (char) statements" in {
        reset()
        var identifier = IdentNode("x")(0, 0)
        identifier.typeId = Some(CharType())
        var node: StatListNode =
            StatListNode(List(ReadNode(identifier)(0, 0)))(0, 0)
        var st = SymbolTable()
        st.add("x", CharType())
        sf = StackFrame(st)
        sf.unlock("x")

        val readCharInstructions: List[Instruction] = List(
          Label("p_read_char"),
          PushInstr(List(lr)),
          MoveInstr(r1, RegOp(r0)),
          LoadLabelInstr(r0, "msg_0"),
          AddInstr(r0, r0, ImmOffset(4)),
          BranchLinkInstr("scanf"),
          PopInstr(List(pc))
        )

        testStat(
          node,
          expectedBssSection ++:
              expectedDataSection(
                List(expectedCharDirective)
              ) ++: expectedTextSection(
                List(
                  List(
                    AddInstr(r0, sp, ImmOffset(0)),
                    BranchLinkInstr("p_read_char")
                  ) ++: readCharInstructions
                )
              )
        )
    }

    it should "translate return statements" in {
        reset()
        var node =
            StatListNode(List(ReturnNode(IntLiterNode(1)(0, 0))(0, 0)))(0, 0)
        testStat(
          node,
          expectedBssSection ++:
              List(
                LoadImmIntInstr(r0, 1),
                PopInstr(List(pc))
              )
        )
    }

    it should "translate begin-end statements" in {
        reset()
        var node =
            StatListNode(
              List(
                BeginEndNode(StatListNode(List(SkipNode()(0, 0)))(0, 0))(0, 0)
              )
            )(0, 0)
        testStat(node, expectedBssSection)
    }

    it should "translate do-while statements (false conditional)" in {
        reset()
        var loopBody = List(Label("wd_0"))
        var whileLoopInstr = List(
          Label("wd_1"),
          MoveInstr(r0, ImmOffset(0)),
          CompareInstr(r0, ImmOffset(1)),
          BranchInstr("wd_0", Condition.EQ)
        )
        var node = StatListNode(
          List(
            WhileDoNode(
              BoolLiterNode(false)(0, 0),
              StatListNode(List(SkipNode()(0, 0)))(0, 0)
            )(0, 0)
          )
        )(0, 0)
        testStat(
          node,
          expectedBssSection ++: List(
            BranchInstr("wd_1")
          ) ++: loopBody ++: whileLoopInstr
        )
    }

    it should "translate do-while statements (true conditional)" in {
        reset()
        var loopBody = List(Label("wd_0"))
        var whileLoopInstr = List(
          Label("wd_1"),
          MoveInstr(r0, ImmOffset(1)),
          CompareInstr(r0, ImmOffset(1)),
          BranchInstr("wd_0", Condition.EQ)
        )
        var node = StatListNode(
          List(
            WhileDoNode(
              BoolLiterNode(true)(0, 0),
              StatListNode(List(SkipNode()(0, 0)))(0, 0)
            )(0, 0)
          )
        )(0, 0)
        testStat(
          node,
          expectedBssSection ++: List(
            BranchInstr("wd_1")
          ) ++: loopBody ++: whileLoopInstr
        )
    }
    it should "translate print reference statements" in {
        reset()
        val st = SymbolTable()
        st.add(
          "anArray",
          Variable(ArrayType(IntType(), List(3), 1))
        )
        sf = StackFrame(st)
        sf.unlock("anArray")
        var node =
            StatListNode(
              List(PrintNode(IdentNode("anArray")(0, 0))(0, 0))
            )(0, 0)
        testStat(
          node,
          expectedBssSection ++:
              expectedDataSection(List(expectedPrintRefDirective))
              ++: List(
                LoadInstr(r0, sp, ImmOffset(0)),
                BranchLinkInstr("p_print_reference")
              )
              ++: expectedPrintRefText(0)
        )
    }
    it should "translate println statements" in {
        reset()
        var node =
            StatListNode(
              List(PrintlnNode(CharLiterNode('c')(0, 0))(0, 0))
            )(0, 0)
        testStat(
          node,
          expectedBssSection ++:
              expectedDataSection(List(expectedPrintLnDirective))
              ++: List(
                MoveInstr(r0, ImmOffset('c')),
                BranchLinkInstr("putchar"),
                BranchLinkInstr("p_print_ln")
              )
              ++: expectedPrintLnText(0)
        )
    }

    it should "translate if then else statements" in {
        reset()
        var node =
            StatListNode(
              List(
                IfThenElseNode(
                  BoolLiterNode(true)(0, 0),
                  StatListNode(
                    List(SkipNode()(0, 0))
                  )(0, 0),
                  StatListNode(
                    List(SkipNode()(0, 0))
                  )(0, 0)
                )(0, 0)
              )
            )(0, 0)
        testStat(
          node,
          expectedBssSection ++:
              List(
                MoveInstr(r0, ImmOffset(1)),
                CompareInstr(r0, ImmOffset(0)),
                BranchInstr("ite_0", Condition.EQ),
                BranchInstr("ite_1"),
                Label("ite_0"),
                Label("ite_1")
              )
        )
    }

    behavior of "Function code generation"
    it should "translate functions that end in return" in {
        reset()
        var node = FuncNode(
          IntTypeNode()(0, 0),
          IdentNode("x")(0, 0),
          List().empty,
          StatListNode(List(ReturnNode(IntLiterNode(1)(0, 0))(0, 0)))(0, 0)
        )(0, 0)
        testFunction(
          node,
          expectedBssSection ++:
              List(
                Label("f_x"),
                PushInstr(List(lr)),
                LoadImmIntInstr(r0, 1),
                PopInstr(List(pc)),
                Directive("ltorg")
              )
        )
        reset()
        node = FuncNode(
          IntTypeNode()(0, 0),
          IdentNode("x")(0, 0),
          List().empty,
          StatListNode(
            List(
              ReturnNode(IntLiterNode(1)(0, 0))(0, 0),
              ReturnNode(IntLiterNode(5)(0, 0))(0, 0)
            )
          )(0, 0)
        )(0, 0)
        testFunction(
          node,
          expectedBssSection ++:
              List(
                Label("f_x"),
                PushInstr(List(lr)),
                LoadImmIntInstr(r0, 1),
                PopInstr(List(pc)),
                LoadImmIntInstr(r0, 5),
                PopInstr(List(pc)),
                Directive("ltorg")
              )
        )
    }
    it should "translate functions that end in exit" in {

        reset()
        var node = FuncNode(
          IntTypeNode()(0, 0),
          IdentNode("x")(0, 0),
          List().empty,
          StatListNode(List(ExitNode(IntLiterNode(1)(0, 0))(0, 0)))(0, 0)
        )(0, 0)
        testFunction(
          node,
          expectedBssSection ++:
              List(
                Label("f_x"),
                PushInstr(List(lr)),
                LoadImmIntInstr(r0, 1),
                BranchLinkInstr("exit"),
                Directive("ltorg")
              )
        )
        reset()
        node = FuncNode(
          IntTypeNode()(0, 0),
          IdentNode("x")(0, 0),
          List().empty,
          StatListNode(
            List(
              ExitNode(IntLiterNode(1)(0, 0))(0, 0),
              ExitNode(IntLiterNode(255)(0, 0))(0, 0)
            )
          )(0, 0)
        )(0, 0)
        testFunction(
          node,
          expectedBssSection ++:
              List(
                Label("f_x"),
                PushInstr(List(lr)),
                LoadImmIntInstr(r0, 1),
                BranchLinkInstr("exit"),
                LoadImmIntInstr(r0, 255),
                BranchLinkInstr("exit"),
                Directive("ltorg")
              )
        )
    }
    behavior of "AssignLHS Code Generation"
    it should "translate identifiers" in {
        reset()
        val node: IdentNode = IdentNode("x")(0, 0)
        node.typeId = Some(IntType())
        val st = SymbolTable()
        st.add("x", IntType())
        sf = StackFrame(st)
        sf.unlock("x")
        testLHS(
          node,
          expectedBssSection
        )
    }
    it should "translate array elements" in {
        reset()
        var node: AssignLHSNode =
            ArrayElemNode(
              IdentNode("arr")(0, 0),
              List(IntLiterNode(0)(0, 0))
            )(0, 0)
        node.typeId = Some(IntType())
        var st = SymbolTable()
        st.add("arr", ArrayType(IntType(), List(1), 1))
        sf = StackFrame(st)
        sf.unlock("arr")

        testLHS(
          node,
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedArrayNegIndexDirective,
                  expectedArrayIndexTooLargeDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    PushInstr(List(r0, r4)),
                    LoadInstr(r4, sp, ImmOffset(8), Condition.AL),
                    LoadImmIntInstr(r0, 0, Condition.AL),
                    BranchLinkInstr("p_check_array_bounds", Condition.AL),
                    AddInstr(r4, r4, ImmOffset(4), false),
                    AddInstr(r4, r4, LSLRegOp(r0, ShiftImm(2)), false),
                    MoveInstr(r1, RegOp(r4), Condition.AL),
                    PopInstr(List(r0, r4))
                  ),
                  expectedCheckArrayBoundsText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(2)
                )
              )
        )
    }

    it should "translate pair elements" in {
        reset()
        var node: PairElemNode = FirstPairElemNode(IdentNode("x")(0, 0))(0, 0)
        node.typeId = Some(IntType())
        var st = SymbolTable()
        st.add("x", PairType(IntType(), CharType()))
        sf = StackFrame(st)
        sf.unlock("x")

        testLHS(
          node,
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedNullReferenceDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    PushInstr(List(r0)),
                    LoadInstr(r0, sp, ImmOffset(4), Condition.AL),
                    BranchLinkInstr("p_check_null_pointer", Condition.AL),
                    AddInstr(r0, r0, ImmOffset(0), false),
                    PushInstr(List(r0)),
                    LoadInstr(r0, r0, ImmOffset(0), Condition.AL),
                    BranchLinkInstr("free", Condition.AL),
                    MoveInstr(r0, ImmOffset(4), Condition.AL),
                    BranchLinkInstr("malloc", Condition.AL),
                    PopInstr(List(r1)),
                    StoreInstr(r0, r1, ImmOffset(0), false),
                    MoveInstr(r1, RegOp(r0), Condition.AL),
                    PopInstr(List(r0))
                  ),
                  expectedNullPointerText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )

        reset()
        node = SecondPairElemNode(IdentNode("x")(0, 0))(0, 0)
        node.typeId = Some(CharType())
        st = SymbolTable()
        st.add("x", PairType(IntType(), CharType()))
        sf = StackFrame(st)
        sf.unlock("x")
        testLHS(
          node,
          expectedBssSection ++:
              expectedDataSection(
                List(
                  expectedNullReferenceDirective,
                  expectedPrintStrDirective
                )
              ) ++:
              expectedTextSection(
                List(
                  List(
                    PushInstr(List(r0)),
                    LoadInstr(r0, sp, ImmOffset(4), Condition.AL),
                    BranchLinkInstr("p_check_null_pointer", Condition.AL),
                    AddInstr(r0, r0, ImmOffset(4), false),
                    PushInstr(List(r0)),
                    LoadInstr(r0, r0, ImmOffset(0), Condition.AL),
                    BranchLinkInstr("free", Condition.AL),
                    MoveInstr(r0, ImmOffset(1), Condition.AL),
                    BranchLinkInstr("malloc", Condition.AL),
                    PopInstr(List(r1)),
                    StoreInstr(r0, r1, ImmOffset(0), false),
                    MoveInstr(r1, RegOp(r0), Condition.AL),
                    PopInstr(List(r0))
                  ),
                  expectedNullPointerText(0),
                  expectedRuntimeErrText,
                  expectedPrintStrText(1)
                )
              )
        )
    }

}
