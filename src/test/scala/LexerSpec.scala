import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Parsley, Parsley._

class LexerSpec extends AnyFlatSpec {
    import parsley.{Success, Failure}

    behavior of "<char-liter> parser combinator"
    it should "parse a character contained in single quotations" in {
        lexer.charLiter.parse("'c'") match {
            case Success(x) => succeed
            case Failure(err) => fail(err)
        }

        lexer.charLiter.parse("'\\0'") match {
            case Success(x) => succeed
            case Failure(err) => fail(err)
        }

        lexer.charLiter.parse("'\\\''") match {
            case Success(x) => succeed
            case Failure(err) => fail(err)
        }
    }

    it should "fail on an empty character literal" in {
        lexer.charLiter.parse("''") match {
            case Success(x) => fail("did not fail on an empty character")
            case Failure(err) => succeed
        }
    }

    it should "fail on un-escaped \\, \', \"" in {
        lexer.charLiter.parse("'\\'") match {
            case Success(x) => fail("did not fail on '\\'")
            case Failure(err) => succeed
        }

        lexer.charLiter.parse("'\''") match {
            case Success(x) => fail("did not fail on '\''")
            case Failure(err) => succeed
        }

        lexer.charLiter.parse("'\"'") match {
            case Success(x) => fail("did not fail on '\"'")
            case Failure(err) => succeed
        }
    }

    it should "fail on multiple characters in single quotations" in {
        lexer.charLiter.parse("'hello'") match {
            case Success(x) => fail("did not fail on multiple characters in single quotes")
            case Failure(err) => succeed
        }
    }

    //it should "parse expressions"

    //it should "ignore whitespace" 

    //it should "ignore comments"


}
