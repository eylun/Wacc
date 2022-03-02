import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class CodeGenSpec extends AnyFlatSpec {
    import parsley.{Success, Failure}
    import testUtils.{assertCodegenEquals}

    var sf: StackFrame = StackFrame(SymbolTable())
    implicit var wbuffer: WaccBuffer = new WaccBuffer
    
    def reset(): Unit = {
        sf = StackFrame(SymbolTable())
        wbuffer = new WaccBuffer
    }

    def testExpr(node: ExprNode, expected: List[Instruction]) = {
        transExpression(node, sf)
        assertCodegenEquals(expected, wbuffer.emit())
    }

    // TODO: Change WaccBuffer to initialize the header stuff separately, then uncomment the test
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
}
