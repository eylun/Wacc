import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._

class LexerSpec extends AnyFlatSpec {
    import parsley.{Result, Success, Failure}

    def assertResultEquals[A](
        expected: Result[String, A],
        actual: Result[String, A]
    ) = {
        expected match {
            case Success(x) => {
                actual match {
                    case Success(y)   => checkTokenMatch(x, y)
                    case Failure(err) => fail(err)
                }
            }
            case Failure(_) => {
                actual match {
                    case Success(s) =>
                        fail("did not fail as expected, actual: " + s)
                    case Failure(_) => succeed
                }
            }
        }
    }

    def checkTokenMatch[A](expected: A, actual: A) = {
        if (expected == actual) succeed
        else
            fail(
              "matched incorrect token (expected: " + expected + ", actual: " + actual + ")"
            )
    }

    behavior of "<bool-liter> parser combinator"
    it should "parse only the 'true' and 'false' keywords" in {
        assertResultEquals(
          Success(BoolLiterNode(true)),
          lexer.boolLiter.parse("true")
        )
        assertResultEquals(
          Success(BoolLiterNode(false)),
          lexer.boolLiter.parse("false")
        )
        assertResultEquals(Failure(""), lexer.boolLiter.parse("tru"))
        assertResultEquals(Failure(""), lexer.boolLiter.parse("truee"))
        assertResultEquals(Failure(""), lexer.boolLiter.parse("fasle"))
    }

    behavior of "<pair-liter> parser combinator"
    it should "parse only the 'null' keyword" in {
        assertResultEquals(
          Success(PairLiterNode()),
          lexer.pairLiter.parse("null")
        )
        assertResultEquals(Failure(""), lexer.pairLiter.parse("nul"))
        assertResultEquals(Failure(""), lexer.pairLiter.parse("nulll"))
    }

    behavior of "<int-liter> parser combinator"
    it should "parse a sign followed by a sequence of digits" in {
        assertResultEquals(
          Success(IntLiterNode(12345)),
          lexer.intLiter.parse("+12345")
        )
        assertResultEquals(
          Success(IntLiterNode(-1232)),
          lexer.intLiter.parse("-1232")
        )
        assertResultEquals(Failure(""), lexer.intLiter.parse("-"))
        assertResultEquals(Failure(""), lexer.intLiter.parse("+"))
        assertResultEquals(Failure(""), lexer.intLiter.parse("+ 04"))
    }
    it should "parse a sequence of digits" in {
        assertResultEquals(
          Success(IntLiterNode(42)),
          lexer.intLiter.parse("42")
        )
        assertResultEquals(
          Success(IntLiterNode(868)),
          lexer.intLiter.parse("868 34")
        )
    }

    behavior of "<char-liter> parser combinator"
    it should "parse a character contained in single quotations" in {
        assertResultEquals(
          Success(CharLiterNode('c')),
          lexer.charLiter.parse("'c'")
        )
        assertResultEquals(
          Success(CharLiterNode('\u0000')),
          lexer.charLiter.parse("'\\0'")
        )
        assertResultEquals(
          Success(CharLiterNode('\\')),
          lexer.charLiter.parse("'\\\\'")
        )
    }
    it should "fail on an empty character literal" in {
        assertResultEquals(Failure(""), lexer.charLiter.parse("''"))
    }
    it should "fail on un-escaped \\, \', \"" in {
        assertResultEquals(Failure(""), lexer.charLiter.parse("'\\'"))
        assertResultEquals(Failure(""), lexer.charLiter.parse("'\''"))
        assertResultEquals(Failure(""), lexer.charLiter.parse("'\"'"))
    }
    it should "fail on multiple characters in single quotations" in {
        assertResultEquals(Failure(""), lexer.charLiter.parse("'hello'"))
    }
    it should "fail on unmatched single quotations" in {
        assertResultEquals(Failure(""), lexer.charLiter.parse("'a"))
        assertResultEquals(Failure(""), lexer.charLiter.parse("''b'"))
    }

    behavior of "<str-liter> parser combinator"
    it should "parse an empty string" in {
        assertResultEquals(
          Success(StringLiterNode("")),
          lexer.stringLiter.parse("\"\"")
        )
    }
    it should "parse a sequence of characters contained in double quotations" in {
        assertResultEquals(
          Success(StringLiterNode("hello world!\n")),
          lexer.stringLiter.parse("\"hello world!\\n\"")
        )
    }
    it should "fail with strings containing illegal unescaped characters" in {
        assertResultEquals(
          Failure(""),
          lexer.stringLiter.parse("\"hello \\ there\"")
        )
    }

    behavior of "<ident> parser combinator"
    it should "parse any legal identifier string" in {
        assertResultEquals(
          Success(IdentNode("camelCaseIdent")),
          lexer.identifier.parse("camelCaseIdent")
        )
        assertResultEquals(
          Success(IdentNode("_underscorewithlowercase")),
          lexer.identifier.parse("_underscorewithlowercase")
        )
        assertResultEquals(
          Success(IdentNode("_underscoreWithCamelCase")),
          lexer.identifier.parse("_underscoreWithCamelCase")
        )
        assertResultEquals(
          Success(IdentNode("UppercaseIdent")),
          lexer.identifier.parse("UppercaseIdent")
        )
        assertResultEquals(
          Success(IdentNode("CONST_IDENT")),
          lexer.identifier.parse("CONST_IDENT")
        )
        assertResultEquals(
          Success(IdentNode("_09")),
          lexer.identifier.parse("_09")
        )
        assertResultEquals(
          Success(IdentNode("snake_case")),
          lexer.identifier.parse("snake_case")
        )
        assertResultEquals(
          Success(IdentNode("combinedIdent_with_Nums_230")),
          lexer.identifier.parse("combinedIdent_with_Nums_230")
        )
        assertResultEquals(
          Success(IdentNode("_____")),
          lexer.identifier.parse("_____")
        )
    }
    it should "fail with a non-alphabetic/underscore first character" in {
        assertResultEquals(
          Failure(""),
          lexer.identifier.parse("10_startsWithNumber")
        )
        assertResultEquals(Failure(""), lexer.identifier.parse("12345"))
        assertResultEquals(
          Failure(""),
          lexer.identifier.parse("!!_starts_with_exclamation")
        )
    }
}
