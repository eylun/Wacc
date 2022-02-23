import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._

// case IntLiterNode(n) =>
// 	collector.addStatement(List(LoadImmIntInstr(Reg(0), n)))
// case CharLiterNode(c) =>
// 	collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(c))))
// case BoolLiterNode(true) =>
// 	collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(1))))
// case BoolLiterNode(false) =>
// 	collector.addStatement(List(MoveInstr(Reg(0), ImmOffset(0))))

class CodegenSpec extends AnyFlatSpec {
    import parsley.{Success, Failure}
    import testUtils.{assertCodegenEquals}
    val sf: StackFrame = StackFrame(SymbolTable())
    // TODO: Change WaccBuffer to initialize the header stuff separately, then uncomment the test
    // behavior of "translating expressions"
    // it should "translate integer literals" in {
    //     implicit val wbuffer: WaccBuffer = new WaccBuffer
    //     transExpression(IntLiterNode(1)(0, 0), sf)
    //     assertCodegenEquals(
    //       List(LoadImmIntInstr(Reg(0), 1)),
    //       wbuffer.emit()
    //     )
    // }
}
