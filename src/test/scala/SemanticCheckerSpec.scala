import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable.ListBuffer
import testUtils.{assertTypeIdEquals}

class SemanticCheckerSpec extends AnyFlatSpec {
    var st: SymbolTable = SymbolTable()
    var log: ListBuffer[WaccError] = ListBuffer()
    var node: ASTNode = IntTypeNode()((0,0))

    def resetNode(n: ASTNode): Unit = {
        this.st = SymbolTable()
        this.log = ListBuffer()
        this.node = n
    }

    behavior of "type check of <type> nodes"
    it should "correctly interpret <base-type> node types" in {
        resetNode(IntTypeNode()((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

        resetNode(BoolTypeNode()((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(BoolType()), node.typeId, ListBuffer(), log)

        resetNode(CharTypeNode()((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(CharType()), node.typeId, ListBuffer(), log)
        
        resetNode(StringTypeNode()((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(StringType()), node.typeId, ListBuffer(), log)
    }
    it should "correctly interpret <pair-type> node types" in {
        // Pair of 2 base types
        resetNode(PairTypeNode(IntTypeNode()((0,0)), 
                                BoolTypeNode()((0,0)))((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(PairType(IntType(), BoolType())), node.typeId,
                            ListBuffer(), log)

        // Pair with nested pairs
        resetNode(PairTypeNode(PairElemTypePairNode()((0,0)), 
                                PairElemTypePairNode()((0,0)))((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(PairType(NestedPairType(), NestedPairType())),
                            node.typeId, ListBuffer(), log)

        // Pair with array
        // resetNode(PairTypeNode(ArrayTypeNode(IntTypeNode()((0,0)), 3)((0,0)),
        //                         StringTypeNode()((0,0)))((0,0)))
        // assertTypeIdEquals(Some(PairType(ArrayType(IntType(), 3), StringType())),
        //                     node.typeId, ListBuffer(), log)
    }
}
