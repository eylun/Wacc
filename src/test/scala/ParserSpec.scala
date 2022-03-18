import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._

class ParserSpec extends AnyFlatSpec {
    import parsley.{Success, Failure}
    import testUtils.{assertResultEquals}

    behavior of "<expr> parsing"
    it should "parse literals (int, bool, char, string, pair)" in {
        assertResultEquals(
          Success(IntLiterNode(-3092)(0, 0)),
          syntax.expr.parse("-3092")
        )
        assertResultEquals(
          Success(BoolLiterNode(true)(0, 0)),
          syntax.expr.parse("true")
        )
        assertResultEquals(
          Success(CharLiterNode('e')(0, 0)),
          syntax.expr.parse("'e'")
        )
        assertResultEquals(
          Success(StringLiterNode("a string")(0, 0)),
          syntax.expr.parse("\"a string\"")
        )
        assertResultEquals(
          Success(new PairLiterNode()(0, 0)),
          syntax.expr.parse("null")
        )
    }
    it should "parse identifiers" in {
        assertResultEquals(
          Success(IdentNode("_some_identifier_22")(0, 0)),
          syntax.expr.parse("_some_identifier_22")
        )
    }
    it should "parse array references" in {
        assertResultEquals(
          Success(
            ArrayElemNode(
              IdentNode("anArray")(0, 0),
              List(IntLiterNode(3)(0, 0))
            )(0, 0)
          ),
          syntax.expr.parse("anArray[3]")
        )
    }
    it should "parse unary operations" in {
        assertResultEquals(
          Success(Not(BoolLiterNode(true)(0, 0))(0, 0)),
          syntax.expr.parse("!true")
        )
        assertResultEquals(
          Success(Len(IdentNode("someArray")(0, 0))(0, 0)),
          syntax.expr.parse("len someArray")
        )
        assertResultEquals(
          Success(Ord(CharLiterNode('A')(0, 0))(0, 0)),
          syntax.expr.parse("ord 'A'")
        )
        assertResultEquals(
          Success(Chr(IntLiterNode(95)(0, 0))(0, 0)),
          syntax.expr.parse("chr 95")
        )

        assertResultEquals(
          Success(Neg(IdentNode("x")(0, 0))(0, 0)),
          syntax.expr.parse("-x")
        )

    }
    it should "parse binary operations" in {
        assertResultEquals(
          Success(Mult(IntLiterNode(2)(0, 0), IntLiterNode(10)(0, 0))(0, 0)),
          syntax.expr.parse("2 * 10")
        )
        assertResultEquals(
          Success(Div(IntLiterNode(-195)(0, 0), IntLiterNode(40)(0, 0))(0, 0)),
          syntax.expr.parse("-195/40")
        )
        assertResultEquals(
          Success(Mod(IntLiterNode(23)(0, 0), IntLiterNode(1)(0, 0))(0, 0)),
          syntax.expr.parse("23 % 1")
        )
        assertResultEquals(
          Success(Add(IntLiterNode(30)(0, 0), IntLiterNode(0)(0, 0))(0, 0)),
          syntax.expr.parse("30 +0")
        )
        assertResultEquals(
          Success(Sub(IntLiterNode(20)(0, 0), IntLiterNode(20)(0, 0))(0, 0)),
          syntax.expr.parse("20-20")
        )
        assertResultEquals(
          Success(GT(CharLiterNode('E')(0, 0), CharLiterNode('P')(0, 0))(0, 0)),
          syntax.expr.parse("'E' > 'P'")
        )
        assertResultEquals(
          Success(
            GTE(IntLiterNode(-340)(0, 0), IntLiterNode(-3000)(0, 0))(0, 0)
          ),
          syntax.expr.parse("-340 >= -3000")
        )
        assertResultEquals(
          Success(LT(IntLiterNode(20)(0, 0), IntLiterNode(10)(0, 0))(0, 0)),
          syntax.expr.parse("20<10")
        )
        assertResultEquals(
          Success(
            LTE(CharLiterNode('a')(0, 0), CharLiterNode('a')(0, 0))(0, 0)
          ),
          syntax.expr.parse("'a'<='a'")
        )
        assertResultEquals(
          Success(
            Equal(BoolLiterNode(true)(0, 0), BoolLiterNode(false)(0, 0))(0, 0)
          ),
          syntax.expr.parse("true == false")
        )
        assertResultEquals(
          Success(
            NotEqual(IntLiterNode(3)(0, 0), IntLiterNode(20)(0, 0))(0, 0)
          ),
          syntax.expr.parse("3!= 20")
        )
        assertResultEquals(
          Success(
            And(
              And(BoolLiterNode(true)(0, 0), BoolLiterNode(false)(0, 0))(0, 0),
              BoolLiterNode(true)(0, 0)
            )(0, 0)
          ),
          syntax.expr.parse("true && false && true")
        )
        assertResultEquals(
          Success(
            Or(
              BoolLiterNode(false)(0, 0),
              And(BoolLiterNode(false)(0, 0), BoolLiterNode(true)(0, 0))(0, 0)
            )(0, 0)
          ),
          syntax.expr.parse("false || false && true")
        )
    }
    it should "parse bracketed expressions" in {
        assertResultEquals(
          Success(IntLiterNode(4)(0, 0)),
          syntax.expr.parse("(4)")
        )
        assertResultEquals(
          Success(
            Mult(
              Add(IntLiterNode(3)(0, 0), IntLiterNode(-7)(0, 0))(0, 0),
              IntLiterNode(10)(0, 0)
            )(0, 0)
          ),
          syntax.expr.parse("(3 + -7) * 10")
        )
    }
    it should "parse expressions with correct precedence" in {
        assertResultEquals(
          Success(
            And(
              BoolLiterNode(true)(0, 0),
              Equal(BoolLiterNode(false)(0, 0), BoolLiterNode(false)(0, 0))(
                0,
                0
              )
            )(0, 0)
          ),
          syntax.expr.parse("true && false == false")
        )
        assertResultEquals(
          Success(
            Sub(
              IntLiterNode(3)(0, 0),
              Div(
                Sub(IntLiterNode(3)(0, 0), IntLiterNode(2)(0, 0))(0, 0),
                IntLiterNode(10)(0, 0)
              )(0, 0)
            )(0, 0)
          ),
          syntax.expr.parse("3-(3-2)/10")
        )
        assertResultEquals(
          Success(
            GTE(
              Ord(CharLiterNode('a')(0, 0))(0, 0),
              Sub(IntLiterNode(30)(0, 0), IntLiterNode(10)(0, 0))(0, 0)
            )(0, 0)
          ),
          syntax.expr.parse("ord 'a' >= (30 - 10)")
        )
        assertResultEquals(
          Success(
            NotEqual(
              LTE(
                Sub(IntLiterNode(4)(0, 0), IntLiterNode(3)(0, 0))(0, 0),
                Mult(IntLiterNode(1)(0, 0), IntLiterNode(2)(0, 0))(0, 0)
              )(0, 0),
              BoolLiterNode(true)(0, 0)
            )(0, 0)
          ),
          syntax.expr.parse("4 - 3 <= 1 * 2 != true")
        )
    }

    behavior of "<assign-lhs> parsing"
    it should "parse an identifier" in {
        assertResultEquals(
          Success(IdentNode("identifier01_LHS")(0, 0)),
          syntax.assignLHS.parse("identifier01_LHS")
        )
    }
    it should "parse an array-elem" in {
        assertResultEquals(
          Success(
            ArrayElemNode(IdentNode("_")(0, 0), List(IntLiterNode(0)(0, 0)))(
              0,
              0
            )
          ),
          syntax.assignLHS.parse("_[0]")
        )
        assertResultEquals(
          Success(
            ArrayElemNode(
              IdentNode("arr_3D")(0, 0),
              List(
                IntLiterNode(2)(0, 0),
                IntLiterNode(3)(0, 0),
                IntLiterNode(20)(0, 0)
              )
            )(0, 0)
          ),
          syntax.assignLHS.parse("arr_3D[2][3][20]")
        )
    }
    it should "parse a pair-elem" in {
        assertResultEquals(
          Success(FirstPairElemNode(IdentNode("some_pair")(0, 0))(0, 0)),
          syntax.assignLHS.parse("fst some_pair")
        )
        assertResultEquals(
          Success(SecondPairElemNode(new PairLiterNode()(0, 0))(0, 0)),
          syntax.assignLHS.parse("snd null")
        )
    }

    behavior of "<assign-rhs> parsing"
    it should "parse an expression" in {
        assertResultEquals(
          Success(Add(IntLiterNode(1)(0, 0), IntLiterNode(2)(0, 0))(0, 0)),
          syntax.assignRHS.parse("1 + 2")
        )
    }
    it should "parse an array literal" in {
        assertResultEquals(
          Success(
            ArrayLiterNode(List(IntLiterNode(1)(0, 0), IntLiterNode(2)(0, 0)))(
              0,
              0
            )
          ),
          syntax.assignRHS.parse("[1,2]")
        )
    }
    it should "parse a newpair construction" in {
        assertResultEquals(
          Success(
            NewPairNode(
              Add(IdentNode("x")(0, 0), IntLiterNode(3)(0, 0))(0, 0),
              IntLiterNode(10)(0, 0)
            )(0, 0)
          ),
          syntax.assignRHS.parse("newpair (x + 3, 10)")
        )
    }

    it should "parse a pair-elem" in {
        assertResultEquals(
          Success(FirstPairElemNode(IdentNode("some_pair")(0, 0))(0, 0)),
          syntax.assignRHS.parse("fst some_pair")
        )
        assertResultEquals(
          Success(SecondPairElemNode(new PairLiterNode()(0, 0))(0, 0)),
          syntax.assignRHS.parse("snd null")
        )
    }

    it should "parse a function call" in {
        assertResultEquals(
          Success(
            CallNode(
              IdentNode("f_printLine")(0, 0),
              List(IntLiterNode(13)(0, 0))
            )(0, 0)
          ),
          syntax.assignRHS.parse("call printLine(13)")
        )
    }

    behavior of "<stat> parsing"
    it should "parse a skip statement" in {
        assertResultEquals(
          Success(StatListNode(List(SkipNode()(0, 0)))(0, 0)),
          syntax.stat.parse("skip")
        )
    }

    it should "parse a new assignment/variable declaration" in {
        assertResultEquals(
          Success(
            StatListNode(
              List(
                NewAssignNode(
                  BoolTypeNode()(0, 0),
                  IdentNode("x")(0, 0),
                  BoolLiterNode(false)(0, 0)
                )(0, 0)
              )
            )(0, 0)
          ),
          syntax.stat.parse("bool x = false")
        )
    }

    it should "parse an assignment" in {
        assertResultEquals(
          Success(
            StatListNode(
              List(
                LRAssignNode(IdentNode("y")(0, 0), CharLiterNode('c')(0, 0))(
                  0,
                  0
                )
              )
            )(0, 0)
          ),
          syntax.stat.parse("y = 'c'")
        )
    }

    it should "parse a read statement" in {
        assertResultEquals(
          Success(
            StatListNode(
              List(
                ReadNode(FirstPairElemNode(IdentNode("x")(0, 0))(0, 0))(0, 0)
              )
            )(0, 0)
          ),
          syntax.stat.parse("read fst x")
        )
    }

    it should "parse a free statement" in {
        assertResultEquals(
          Success(
            StatListNode(List(FreeNode(IdentNode("a")(0, 0))(0, 0)))(0, 0)
          ),
          syntax.stat.parse("free a")
        )
    }

    it should "parse a return statement" in {
        assertResultEquals(
          Success(
            StatListNode(
              List(ReturnNode(IntLiterNode(3)(0, 0))(0, 0))
            )(0, 0)
          ),
          syntax.stat.parse("return 3")
        )
    }

    it should "parse a exit statement" in {
        assertResultEquals(
          Success(
            StatListNode(List(ExitNode(IntLiterNode(100)(0, 0))(0, 0)))(0, 0)
          ),
          syntax.stat.parse("exit 100")
        )
    }

    it should "parse a print statement" in {
        assertResultEquals(
          Success(
            StatListNode(
              List(PrintNode(StringLiterNode("hello world")(0, 0))(0, 0))
            )(0, 0)
          ),
          syntax.stat.parse("print \"hello world\"")
        )
    }
    it should "parse a println statement" in {
        assertResultEquals(
          Success(
            StatListNode(List(PrintlnNode(CharLiterNode('c')(0, 0))(0, 0)))(
              0,
              0
            )
          ),
          syntax.stat.parse("println 'c'")
        )
    }

    it should "parse an if-else statement" in {
        assertResultEquals(
          Success(
            StatListNode(
              List(
                IfThenElseNode(
                  Equal(IdentNode("a")(0, 0), IntLiterNode(13)(0, 0))(0, 0),
                  StatListNode(
                    List(PrintlnNode(StringLiterNode("correct")(0, 0))(0, 0))
                  )(0, 0),
                  StatListNode(
                    List(PrintlnNode(StringLiterNode("incorrect")(0, 0))(0, 0))
                  )(0, 0)
                )(0, 0)
              )
            )(0, 0)
          ),
          syntax.stat.parse(
            "if a == 13 " +
                "then " +
                "println \"correct\" " +
                "" +
                "else " +
                "println \"incorrect\" " +
                "fi"
          )
        )
    }

    it should "parse a while-do statement" in {
        assertResultEquals(
          Success(
            StatListNode(
              List(
                WhileDoNode(
                  NotEqual(IdentNode("n")(0, 0), IntLiterNode(1)(0, 0))(0, 0),
                  StatListNode(
                    List(
                      LRAssignNode(IdentNode("n")(0, 0), IntLiterNode(1)(0, 0))(
                        0,
                        0
                      )
                    )
                  )(0, 0)
                )(0, 0)
              )
            )(0, 0)
          ),
          syntax.stat.parse("while n != 1 do n = 1 done")
        )
    }

    it should "parse a begin-end statement" in {
        assertResultEquals(
          Success(
            StatListNode(
              List(
                BeginEndNode(
                  StatListNode(
                    List(
                      SkipNode()(0, 0)
                    )
                  )(0, 0)
                )(0, 0)
              )
            )(0, 0)
          ),
          syntax.stat.parse("begin skip end")
        )
    }

    it should "parse more than one statements" in {
        assertResultEquals(
          Success(
            StatListNode(
              List(
                PrintNode(StringLiterNode("hello")(0, 0))(0, 0),
                SkipNode()(0, 0)
              )
            )(0, 0)
          ),
          syntax.stat.parse("print \"hello\"; skip")
        )
    }

    behavior of "<func> parsing"
    it should "correctly parse a simple function with no params" in {
        assertResultEquals(
          Success(
            FuncNode(
              IntTypeNode()(0, 0),
              IdentNode("f_f")(0, 0),
              List(),
              StatListNode(List(ReturnNode(IntLiterNode(0)(0, 0))(0, 0)))(0, 0)
            )(0, 0)
          ),
          syntax.func.parse("int f() is return 0 end")
        )
    }

    it should "correctly parse a function with parameters" in {
        assertResultEquals(
          Success(
            FuncNode(
              IntTypeNode()(0, 0),
              IdentNode("f_f")(0, 0),
              List(
                ParamNode(IntTypeNode()(0, 0), IdentNode("x")(0, 0))(0, 0),
                ParamNode(BoolTypeNode()(0, 0), IdentNode("y")(0, 0))(0, 0)
              ),
              StatListNode(List(ReturnNode(IdentNode("x")(0, 0))(0, 0)))(0, 0)
            )(0, 0)
          ),
          syntax.func.parse("int f(int x, bool y) is return x end")
        )
    }

    it should "correctly parse a function with multiple statements" in {
        assertResultEquals(
          Success(
            FuncNode(
              CharTypeNode()(0, 0),
              IdentNode("f_f")(0, 0),
              List(),
              StatListNode(
                List(
                  ReturnNode(CharLiterNode('c')(0, 0))(0, 0),
                  ReturnNode(CharLiterNode('d')(0, 0))(0, 0)
                )
              )(0, 0)
            )(0, 0)
          ),
          syntax.func.parse("char f() is return 'c'; return 'd' end")
        )
    }

    behavior of "<array-type> parsing"
    it should "parse any type followed by square brackets" in {
        assertResultEquals(
          Success(ArrayTypeNode(IntTypeNode()(0, 0), 1)(0, 0)),
          syntax.arrayType.parse("int[]")
        )
        assertResultEquals(
          Success(ArrayTypeNode(CharTypeNode()(0, 0), 3)(0, 0)),
          syntax.arrayType.parse("char[][][]")
        )
        assertResultEquals(
          Success(
            ArrayTypeNode(
              PairTypeNode(CharTypeNode()(0, 0), StringTypeNode()(0, 0))(0, 0),
              2
            )(0, 0)
          ),
          syntax.arrayType.parse("pair(char, string)[][]")
        )
    }
    it should "fail on invalid type" in {
        assertResultEquals(Failure(""), syntax.arrayType.parse("pair[]"))
    }

    behavior of "<pair-type> parsing"
    it should "parse pairs with base type elements" in {
        assertResultEquals(
          Success(
            PairTypeNode(IntTypeNode()(0, 0), BoolTypeNode()(0, 0))(0, 0)
          ),
          syntax.anyType.parse("pair (int, bool)")
        )
    }

    it should "parse pairs with array type elements" in {
        assertResultEquals(
          Success(
            PairTypeNode(
              IntTypeNode()(0, 0),
              ArrayTypeNode(CharTypeNode()(0, 0), 1)(0, 0)
            )(0, 0)
          ),
          syntax.anyType.parse("pair (int, char[])")
        )

        assertResultEquals(
          Success(
            PairTypeNode(
              ArrayTypeNode(IntTypeNode()(0, 0), 1)(0, 0),
              ArrayTypeNode(BoolTypeNode()(0, 0), 2)(0, 0)
            )(0, 0)
          ),
          syntax.anyType.parse("pair (int[], bool[][])")
        )

    }

    it should "parse pairs with pair type elements" in {
        assertResultEquals(
          Success(
            PairTypeNode(IntTypeNode()(0, 0), PairElemTypePairNode()(0, 0))(
              0,
              0
            )
          ),
          syntax.anyType.parse("pair (int, pair)")
        )

        assertResultEquals(
          Success(
            PairTypeNode(
              PairElemTypePairNode()(0, 0),
              PairElemTypePairNode()(0, 0)
            )(0, 0)
          ),
          syntax.anyType.parse("pair (pair, pair)")
        )

    }

    behavior of "<basic-type> parsing"
    it should "parse all basic types" in {
        assertResultEquals(
          Success(IntTypeNode()(0, 0)),
          syntax.anyType.parse("int")
        )

        assertResultEquals(
          Success(BoolTypeNode()(0, 0)),
          syntax.anyType.parse("bool")
        )
        assertResultEquals(
          Success(CharTypeNode()(0, 0)),
          syntax.anyType.parse("char")
        )
        assertResultEquals(
          Success(StringTypeNode()(0, 0)),
          syntax.anyType.parse("string")
        )
    }
}
