import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable.ListBuffer
import testUtils.{assertTypeIdEquals}

class SemanticCheckerSpec extends AnyFlatSpec {
  behavior of "<type> nodes"
    it should "interpret the correct type associated with the node" in {
        val st = SymbolTable()
        val log = ListBuffer[WaccError]()
        val node = IntTypeNode()((0,0))

        node.check(st, log)

        assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)
    }
}
