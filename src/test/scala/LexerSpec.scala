import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._

class LexerSpec extends AnyFlatSpec {
    import parsley.{Success, Failure}

    behavior of "<bool-liter> parser combinator"
    it should "parse only the 'true' and 'false' keywords" in {
        lexer.boolLiter.parse("true") match {
            case Success(BoolLiterNode(true)) => succeed
            case Success(x)   => fail("matched on incorrect token (expected: BoolLiterNode(true), was: " + x + ")")
            case Failure(err) => fail(err)
        }
        lexer.boolLiter.parse("false") match {
            case Success(BoolLiterNode(false))   => succeed
            case Success(x) => fail("matched incorrect token (expected: BoolLiterNode(false), was: " + x + ")")
            case Failure(err) => fail(err)
        }
        lexer.boolLiter.parse("tru") match {
            case Success(_) => fail("matched on misspelled 'tru'")
            case Failure(_) => succeed
        }
        lexer.boolLiter.parse("truee") match {
            case Success(_) => fail("matched on misspelled 'truee'")
            case Failure(_) => succeed
        }
        lexer.boolLiter.parse("fasle") match {
            case Success(_) => fail("matched on misspelled 'fasle'")
            case Failure(_) => succeed
        }
    }

    behavior of "<pair-liter> parser combinator"
    it should "parse only the 'null' keyword" in {
        lexer.pairLiter.parse("null") match {
            case Success(PairLiterNode()) => succeed
            case Failure(err)             => fail(err)
        }
        lexer.pairLiter.parse("nul") match {
            case Success(_) => fail("matched on misspelled 'nul'")
            case Failure(_) => succeed
        }
        lexer.pairLiter.parse("nulll") match {
            case Success(_) => fail("matched on misspelled 'nulll'")
            case Failure(_) => succeed
        }
    }

    behavior of "<int-liter> parser combinator"
    it should "parse a sign followed by a sequence of digits" in {
        lexer.intLiter.parse("+12345") match {
            case Success(IntLiterNode(12345)) => succeed
            case Success(x)   => fail("matched incorrect token (expected: IntLiterNode(12345), was: " + x + ")")
            case Failure(err) => fail(err)
        }
        lexer.intLiter.parse("-1232") match {
            case Success(IntLiterNode(-1232)) => succeed
            case Success(x)   => fail("matched incorrect token (expected: IntLiterNode(-1232), was: " + x + ")")
            case Failure(err) => fail(err)
        }
        lexer.intLiter.parse("-") match {
            case Success(_) => fail("matched on - with no digits after it")
            case Failure(_) => succeed
        }
        lexer.intLiter.parse("+") match {
            case Success(_) => fail("matched on + with no digits after it")
            case Failure(_) => succeed
        }
        lexer.intLiter.parse("+ 04") match {
            case Success(_) => fail("matched on number with space after sign")
            case Failure(_) => succeed
        }
    }
    it should "parse a sequence of digits" in {
        lexer.intLiter.parse("42") match {
            case Success(IntLiterNode(42)) => succeed
            case Success(x)   => fail("matched incorrect token (expected: IntLiterNode(42), was: " + x + ")")
            case Failure(err) => fail(err)
        }
        lexer.intLiter.parse("868 34") match {
            case Success(IntLiterNode(868)) => succeed
            case _ => fail("did not match only the first sequence")
        }
    }

    behavior of "<char-liter> parser combinator"
    it should "parse a character contained in single quotations" in {
        lexer.charLiter.parse("'c'") match {
            case Success(CharLiterNode('c')) => succeed
            case Success(x)   => fail("matched incorrect token (expected: CharLiterNode('c'), was: " + x + ")")
            case Failure(err) => fail(err)
        }
        lexer.charLiter.parse("'\\0'") match {
            case Success(CharLiterNode('\u0000')) => succeed
            case Success(x)   => fail("matched incorrect token (expected: CharLiterNode('\u0000'), was: " + x + ")")
            case Failure(err) => fail(err)
        }
        lexer.charLiter.parse("'\\\\'") match {
            case Success(CharLiterNode('\\')) => succeed
            case Success(x)   => fail("matched incorrect token (expected: CharLiterNode('\\'), was: " + x + ")")
            case Failure(err) => fail(err)
        }
    }
    it should "fail on an empty character literal" in {
        lexer.charLiter.parse("''") match {
            case Success(_) => fail("did not fail on an empty character")
            case Failure(_) => succeed
        }
    }
    it should "fail on un-escaped \\, \', \"" in {
        lexer.charLiter.parse("'\\'") match {
            case Success(_) => fail("did not fail on '\\'")
            case Failure(_) => succeed
        }
        lexer.charLiter.parse("'\''") match {
            case Success(_) => fail("did not fail on '\''")
            case Failure(_) => succeed
        }
        lexer.charLiter.parse("'\"'") match {
            case Success(_) => fail("did not fail on '\"'")
            case Failure(_) => succeed
        }
    }
    it should "fail on multiple characters in single quotations" in {
        lexer.charLiter.parse("'hello'") match {
            case Success(_) =>
                fail("matched on multiple characters in single quotes")
            case Failure(_) => succeed
        }
    }
    it should "fail on unmatched single quotations" in {
        lexer.charLiter.parse("'a") match {
            case Success(_) => fail("did not fail on missing single quote")
            case Failure(_) => succeed
        }
        lexer.charLiter.parse("''b'") match {
            case Success(_) => fail("did not fail on extra single quote")
            case Failure(_) => succeed
        }
    }

    behavior of "<str-liter> parser combinator"
    it should "parse an empty string" in {
        lexer.stringLiter.parse("\"\"") match {
            case Success(StringLiterNode("")) => succeed
            case Success(x)   => fail("matched incorrect token (expected: StringLiterNode(\"\"), was: " + x + ")") 
            case Failure(err) => fail(err)
        }
    }
    it should "parse a sequence of characters contained in double quotations" in {
        lexer.stringLiter.parse("\"hello world!\\n\"") match {
            case Success(StringLiterNode("hello world!\n")) => succeed
            case Success(x)   => fail("matched incorrect token (expected: StringLiterNode(\"hello world!\\\n\"), was: " + x + ")")
            case Failure(err) => fail(err)
        }
    }
    it should "fail with strings containing illegal unescaped characters" in {
        lexer.stringLiter.parse("\"hello \\ there\"") match {
            case Success(_) => fail("matched with unescaped \\ in string")
            case Failure(_) => succeed
        }
    }

    //it should "parse expressions"

    //it should "ignore whitespace"

    //it should "ignore comments"
}
