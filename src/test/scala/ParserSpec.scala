import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._

class ParserSpec extends AnyFlatSpec {
    import parsley.{Success, Failure}
    import testUtils.{assertResultEquals}

    // behavior of "<expr> parsing"
    // it should "parse literals (int, bool, char, string, pair)" in {
    //     assertResultEquals(
    //       Success(IntLiterNode(-3092)(_)),
    //       syntax.expr.parse("-3092")
    //     )
    //     assertResultEquals(
    //       Success(BoolLiterNode(true)(_)),
    //       syntax.expr.parse("true")
    //     )
    //     assertResultEquals(
    //       Success(CharLiterNode('e')(_)),
    //       syntax.expr.parse("'e'")
    //     )
    //     assertResultEquals(
    //       Success(StringLiterNode("a string")(_)),
    //       syntax.expr.parse("\"a string\"")
    //     )
    //     assertResultEquals(Success(PairLiterNode()), syntax.expr.parse("null"))
    // }
    // it should "parse identifiers" in {
    //     assertResultEquals(
    //       Success(IdentNode("_some_identifier_22")(_)),
    //       syntax.expr.parse("_some_identifier_22")
    //     )
    // }
    // it should "parse array references" in {
    //     assertResultEquals(
    //       Success(
    //         ArrayElemNode(IdentNode("anArray")(_), List(IntLiterNode(3)(_)))(
    //           _: (Int, Int)
    //         )
    //       ),
    //       syntax.expr.parse("anArray[3]")
    //     )
    // }
    // it should "parse unary operations" in {
    //     assertResultEquals(
    //       Success(Not(BoolLiterNode(true)(_))),
    //       syntax.expr.parse("!true")
    //     )
    //     assertResultEquals(
    //       Success(Len(IdentNode("someArray")(_))),
    //       syntax.expr.parse("len someArray")
    //     )
    //     assertResultEquals(
    //       Success(Ord(CharLiterNode('A')(_))),
    //       syntax.expr.parse("ord 'A'")
    //     )
    //     assertResultEquals(
    //       Success(Chr(IntLiterNode(95)(_))),
    //       syntax.expr.parse("chr 95")
    //     )
    //     // TODO test negation
    // }
    // it should "parse binary operations" in {
    //     assertResultEquals(
    //       Success(Mult(IntLiterNode(2)(_), IntLiterNode(10)(_))),
    //       syntax.expr.parse("2 * 10")
    //     )
    //     assertResultEquals(
    //       Success(Div(IntLiterNode(-195)(_), IntLiterNode(40)(_))),
    //       syntax.expr.parse("-195/40")
    //     )
    //     assertResultEquals(
    //       Success(Mod(IntLiterNode(23)(_), IntLiterNode(1)(_))),
    //       syntax.expr.parse("23 % 1")
    //     )
    //     assertResultEquals(
    //       Success(Add(IntLiterNode(30)(_), IntLiterNode(0)(_))),
    //       syntax.expr.parse("30 +0")
    //     )
    //     assertResultEquals(
    //       Success(Sub(IntLiterNode(20)(_), IntLiterNode(20)(_))),
    //       syntax.expr.parse("20-20")
    //     )
    //     assertResultEquals(
    //       Success(GT(CharLiterNode('E')(_), CharLiterNode('P')(_))),
    //       syntax.expr.parse("'E' > 'P'")
    //     )
    //     assertResultEquals(
    //       Success(GTE(IntLiterNode(-340)(_), IntLiterNode(-3000)(_))),
    //       syntax.expr.parse("-340 >= -3000")
    //     )
    //     assertResultEquals(
    //       Success(LT(IntLiterNode(20)(_), IntLiterNode(10)(_))),
    //       syntax.expr.parse("20<10")
    //     )
    //     assertResultEquals(
    //       Success(LTE(CharLiterNode('a')(_), CharLiterNode('a')(_))),
    //       syntax.expr.parse("'a'<='a'")
    //     )
    //     assertResultEquals(
    //       Success(Equal(BoolLiterNode(true)(_), BoolLiterNode(false)(_))),
    //       syntax.expr.parse("true == false")
    //     )
    //     assertResultEquals(
    //       Success(NotEqual(IntLiterNode(3)(_), IntLiterNode(20)(_))),
    //       syntax.expr.parse("3!= 20")
    //     )
    //     assertResultEquals(
    //       Success(
    //         And(
    //           And(BoolLiterNode(true)(_), BoolLiterNode(false)(_)),
    //           BoolLiterNode(true)(_)
    //         )
    //       ),
    //       syntax.expr.parse("true && false && true")
    //     )
    //     assertResultEquals(
    //       Success(
    //         Or(
    //           BoolLiterNode(false)(_),
    //           And(BoolLiterNode(false)(_), BoolLiterNode(true)(_))
    //         )
    //       ),
    //       syntax.expr.parse("false || false && true")
    //     )
    // }
    // it should "parse bracketed expressions" in {
    //     assertResultEquals(
    //       Success(IntLiterNode(4)(_)),
    //       syntax.expr.parse("(4)")
    //     )
    //     assertResultEquals(
    //       Success(
    //         Mult(
    //           Add(IntLiterNode(3)(_), IntLiterNode(-7)(_)),
    //           IntLiterNode(10)(_)
    //         )
    //       ),
    //       syntax.expr.parse("(3 + -7) * 10")
    //     )
    // }
    // it should "parse expressions with correct precedence" in {
    //     assertResultEquals(
    //       Success(
    //         And(
    //           BoolLiterNode(true)(_),
    //           Equal(BoolLiterNode(false)(_), BoolLiterNode(false)(_))
    //         )
    //       ),
    //       syntax.expr.parse("true && false == false")
    //     )
    //     assertResultEquals(
    //       Success(
    //         Sub(
    //           IntLiterNode(3)(_),
    //           Div(
    //             Sub(IntLiterNode(3)(_), IntLiterNode(2)(_)),
    //             IntLiterNode(10)(_)
    //           )
    //         )
    //       ),
    //       syntax.expr.parse("3-(3-2)/10")
    //     )
    //     assertResultEquals(
    //       Success(
    //         GTE(
    //           Ord(
    //             CharLiterNode('a')(_),
    //             Sub(IntLiterNode(30)(_), IntLiterNode(10)(_))
    //           )
    //         ),
    //         syntax.expr.parse("ord 'a' >= (30 - 10)")
    //       )
    //     )
    //     assertResultEquals(
    //       Success(
    //         NotEqual(
    //           LTE(
    //             Sub(IntLiterNode(4)(_), IntLiterNode(3)(_)),
    //             Mult(IntLiterNode(1)(_), IntLiterNode(2)(_))
    //           ),
    //           BoolLiterNode(true)(_)
    //         )
    //       ),
    //       syntax.expr.parse("4 - 3 <= 1 * 2 != true")
    //     )
    // }

    // behavior of "<assign-lhs> parsing"
    // it should "parse an identifier" in {
    //     assertResultEquals(
    //       Success(IdentNode("identifier01_LHS")(_)),
    //       syntax.assignLHS.parse("identifier01_LHS")
    //     )
    // }
    // it should "parse an array-elem" in {
    //     assertResultEquals(
    //       Success(ArrayElemNode(IdentNode("_")(_), List(IntLiterNode(0)(_)))),
    //       syntax.assignLHS.parse("_[0]")
    //     )
    //     assertResultEquals(
    //       Success(
    //         ArrayElemNode(
    //           IdentNode("arr_3D")(_),
    //           List(IntLiterNode(2)(_), IntLiterNode(3)(_), IntLiterNode(20)(_))
    //         )
    //       ),
    //       syntax.assignLHS.parse("arr_3D[2][3][20]")
    //     )
    // }
    // it should "parse a pair-elem" in {
    //     assertResultEquals(
    //       Success(FirstPairElemNode(IdentNode("some_pair")(_))),
    //       syntax.assignLHS.parse("fst some_pair")
    //     )
    //     assertResultEquals(
    //       Success(SecondPairElemNode(PairLiterNode())),
    //       syntax.assignLHS.parse("snd null")
    //     )
    // }

    // // behavior of "<assign-rhs> parsing"
    // // it should "parse an expression"
    // // it should "parse an array literal"
    // // it should "parse a newpair construction"
    // // it should "parse a pair-elem"
    // // it should "parse a function call"

    // // behavior of "<stat> parsing"

    // // behavior of "<func> parsing"

    // behavior of "<array-type> parsing"
    // it should "parse any type followed by square brackets" in {
    //     assertResultEquals(
    //       Success(ArrayTypeNode(IntTypeNode(), 1)),
    //       syntax.arrayType.parse("int[]")
    //     )
    //     assertResultEquals(
    //       Success(ArrayTypeNode(CharTypeNode(), 3)),
    //       syntax.arrayType.parse("char[][][]")
    //     )
    //     assertResultEquals(
    //       Success(
    //         ArrayTypeNode(PairTypeNode(CharTypeNode(), StringTypeNode()), 2)
    //       ),
    //       syntax.arrayType.parse("pair(char, string)[][]")
    //     )
    // }
    // it should "fail on invalid type" in {
    //     assertResultEquals(Failure(""), syntax.arrayType.parse("pair[]"))
    // }

    // // behavior of "<pair-type> parsing"

    // // behavior of "<type> parsing"
}
