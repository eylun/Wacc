import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable.ListBuffer

class CodeGenSpec extends AnyFlatSpec {
    import parsley.{Success, Failure}
    import testUtils.{assertCodegenEquals}

    var sf: StackFrame = StackFrame(SymbolTable())
    implicit var wbuffer: WaccBuffer = new WaccBuffer
    
    def reset(): Unit = {
        sf = StackFrame(SymbolTable())
        wbuffer = new WaccBuffer
    }

    def testExpr(node: ExprNode, expected: List[Instruction]): Unit = {
        transExpression(node, sf)
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

    val expectedOverflowDirective: List[Instruction] = List(
        Directive("word 83"),
        Directive("ascii \"OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n\\0\"")
    )

    val expectedPrintStrDirective: List[Instruction] = List(
        Directive("word 5"),
        Directive("ascii \"%.*s\\0\"")
    )

    def expectedTextSection(sections: List[List[Instruction]]): List[Instruction] = {
        val expected: ListBuffer[Instruction] = ListBuffer()
        sections.foreach(s => s.foreach(i => expected += i))
        expected.toList
    }

    def expectedOverflowText(msgNo: Int): List[Instruction] = List(
        Label("p_throw_overflow_error"),
        LoadLabelInstr(Reg(0), s"msg_$msgNo"),
        BranchLinkInstr("p_throw_runtime_error")
    )

    val expectedRuntimeErrText: List[Instruction] = List(
        Label("p_throw_runtime_error"),
        BranchLinkInstr("p_print_string"),
        MoveInstr(Reg(0), ImmOffset(-1)),
        BranchLinkInstr("exit")
    )

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
    }
    it should "translate subtraction expressions" in {
    }
}
