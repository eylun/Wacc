import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._

class ParserSpec extends AnyFlatSpec {
    import parsley.{Success, Failure}
    import testUtils.{assertResultEquals}

    behavior of "<expr> parsing"
    it should "parse literals (int, bool, char, string, pair)" in {
        assertResultEquals(
            Success(IntLiterNode(-3092)), 
            syntax.expr.parse("-3092")
        )
        assertResultEquals(
            Success(BoolLiterNode(true)),
            syntax.expr.parse("true")
        )
        assertResultEquals(
            Success(CharLiterNode('e')),
            syntax.expr.parse("'e'")
        )
        assertResultEquals(
            Success(StringLiterNode("a string")),
            syntax.expr.parse("\"a string\"")
        )
        assertResultEquals(Success(PairLiterNode()), syntax.expr.parse("null"))
    }
    it should "parse identifiers" in {
        assertResultEquals(
            Success(IdentNode("_some_identifier_22")),
            syntax.expr.parse("_some_identifier_22")
        )
    }
    it should "parse array references" in {
        assertResultEquals(
            Success(ArrayElemNode(IdentNode("anArray"),
                                List(IntLiterNode(3)))),
            syntax.expr.parse("anArray[3]")
        )
    }
    it should "parse unary operations" in {
        assertResultEquals(
            Success(Not(BoolLiterNode(true))),
            syntax.expr.parse("!true")
        )
        assertResultEquals(
            Success(Len(IdentNode("someArray"))),
            syntax.expr.parse("len someArray")
        )
        assertResultEquals(
            Success(Ord(CharLiterNode('A'))),
            syntax.expr.parse("ord 'A'")
        )
        assertResultEquals(
            Success(Chr(IntLiterNode(95))),
            syntax.expr.parse("chr 95")
        )
        // TODO test negation
    }
    it should "parse binary operations" in {
        assertResultEquals(
            Success(Mult(IntLiterNode(2), IntLiterNode(10))),
            syntax.expr.parse("2 * 10")
        )
        assertResultEquals(
            Success(Div(IntLiterNode(-195), IntLiterNode(40))),
            syntax.expr.parse("-195/40")
        )
        assertResultEquals(
            Success(Mod(IntLiterNode(23), IntLiterNode(1))),
            syntax.expr.parse("23 % 1")
        )
        assertResultEquals(
            Success(Add(IntLiterNode(30), IntLiterNode(0))),
            syntax.expr.parse("30 +0")
        )
        assertResultEquals(
            Success(Sub(IntLiterNode(20), IntLiterNode(20))),
            syntax.expr.parse("20-20")
        )
        assertResultEquals(
            Success(GT(CharLiterNode('E'), CharLiterNode('P'))),
            syntax.expr.parse("'E' > 'P'")
        )
        assertResultEquals(
            Success(GTE(IntLiterNode(-340), IntLiterNode(-3000))),
            syntax.expr.parse("-340 >= -3000")
        )
        assertResultEquals(
            Success(LT(IntLiterNode(20), IntLiterNode(10))),
            syntax.expr.parse("20<10")
        )
        assertResultEquals(
            Success(LTE(CharLiterNode('a'), CharLiterNode('a'))),
            syntax.expr.parse("'a'<='a'")
        )
        assertResultEquals(
            Success(Equal(BoolLiterNode(true), BoolLiterNode(false))),
            syntax.expr.parse("true == false")
        )
        assertResultEquals(
            Success(NotEqual(IntLiterNode(3), IntLiterNode(20))),
            syntax.expr.parse("3!= 20")
        )
        assertResultEquals(
            Success(And(And(BoolLiterNode(true), BoolLiterNode(false)), 
                        BoolLiterNode(true))),
            syntax.expr.parse("true && false && true")
        )
        assertResultEquals(
            Success(Or(BoolLiterNode(false), 
                        And(BoolLiterNode(false), BoolLiterNode(true)))),
            syntax.expr.parse("false || false && true")
        )
    }
    it should "parse bracketed expressions" in {
        assertResultEquals(
            Success(IntLiterNode(4)),
            syntax.expr.parse("(4)")
        )
        assertResultEquals(
            Success(Mult(Add(IntLiterNode(3), IntLiterNode(-7)), 
                            IntLiterNode(10))),
            syntax.expr.parse("(3 + -7) * 10")
        )
    }
    it should "parse expressions with correct precedence" in {
        assertResultEquals(
            Success(And(BoolLiterNode(true), 
                        Equal(BoolLiterNode(false), BoolLiterNode(false)))),
            syntax.expr.parse("true && false == false")
        )
        assertResultEquals(
            Success(Sub(IntLiterNode(3), 
                        Div(Sub(IntLiterNode(3), IntLiterNode(2)), 
                            IntLiterNode(10)))),
            syntax.expr.parse("3-(3-2)/10")
        )
        assertResultEquals(
            Success(GTE(Ord(CharLiterNode('a')), 
                        Sub(IntLiterNode(30), IntLiterNode(10)))),
            syntax.expr.parse("ord 'a' >= (30 - 10)")
        )
        assertResultEquals(
            Success(NotEqual(LTE(Sub(IntLiterNode(4), IntLiterNode(3)), 
                                Mult(IntLiterNode(1), IntLiterNode(2))), 
                            BoolLiterNode(true))),
            syntax.expr.parse("4 - 3 <= 1 * 2 != true")
        )
    }
}
