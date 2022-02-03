import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ParseSpec extends AnyFlatSpec {
  behavior of "Parser Combinator"

  it should "parse literals" in {
    10 should be(10)
  }

  it should "parse expressions"

  it should "ignore whitespace"

  it should "ignore comments"
}
