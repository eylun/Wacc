sealed trait ASTNode

// Program
case class ProgramNode(flist: List[FuncNode], s: StatNode) extends ASTNode

// Function
case class FuncNode(
    t: TypeNode,
    i: IdentNode,
    plist: List[ParamNode],
    s: StatNode
) extends ASTNode

// Param
case class ParamNode(t: TypeNode, i: IdentNode) extends ASTNode

// Statement
sealed trait StatNode extends ASTNode

case class SkipNode() extends StatNode

case class NewAssignNode(t: TypeNode, i: IdentNode, r: AssignRHSNode) extends StatNode {
    def check(st: SymbolTable): Unit = {
        val varName: String = i.s
        var varObj: Option[Variable] = None

        st.lookup(varName) match {
            case None => {
                varObj = Some(Variable(t.typeId))
                st.add(varName, varObj.get)
            }
            case Some(id) => {
                println(varName + " is already declared") // TODO halt semantic checks.
            }
        }
    }
}

case class LRAssignNode(l: AssignLHSNode, r: AssignRHSNode) extends StatNode

case class ReadNode(l: AssignLHSNode) extends StatNode

case class FreeNode(e: ExprNode) extends StatNode

case class ReturnNode(e: ExprNode) extends StatNode

case class ExitNode(e: ExprNode) extends StatNode

case class PrintNode(e: ExprNode) extends StatNode

case class PrintlnNode(e: ExprNode) extends StatNode
case class IfThenElseNode(e: ExprNode, s1: StatNode, s2: StatNode)
    extends StatNode

case class WhileDoNode(e: ExprNode, s: StatNode) extends StatNode

case class BeginEndNode(s: StatNode) extends StatNode

case class StatListNode(s: List[StatNode]) extends StatNode

// Assign LHS
sealed trait AssignLHSNode extends ASTNode

// Assign RHS
sealed trait AssignRHSNode extends ASTNode

case class NewPairNode(e1: ExprNode, e2: ExprNode) extends AssignRHSNode

case class CallNode(i: IdentNode, args: List[ExprNode]) extends AssignRHSNode

// Type
sealed trait TypeNode extends ASTNode {
    val typeId: Type
}

// Base Type
sealed trait BaseTypeNode extends TypeNode with PairElemTypeNode

case class IntTypeNode() extends BaseTypeNode {
    val typeId = IntType(Math.pow(2, -31).toInt, Math.pow(2,31).toInt)
}

case class BoolTypeNode() extends BaseTypeNode {
    val typeId = BoolType()
}

case class CharTypeNode() extends BaseTypeNode {
    val typeId = CharType()
}

case class StringTypeNode() extends BaseTypeNode {
    val typeId = StringType()
}

// Array Type
/* Special Representation: dimension tracks how many dimensions the identifier's
 * array is. This is inserted upon parsing to make it less tedious for
 * semantic checking.
 */
case class ArrayTypeNode(t: TypeNode, dimension: Int = 1)
    extends PairElemTypeNode
    with TypeNode {
        val typeId = ArrayType(t.typeId, 0)
    }

// object ArrayTypeNode {
//     // def apply(t: TypeNode, dimension: Int = 1z): ArrayTypeNode =
//     //     ArrayTypeNode(t, 1)
//     def apply(t: TypeNode, dimension: Int = 1): ArrayTypeNode =
//         ArrayTypeNode(t, dimension)
//     def unapply(a: ArrayTypeNode): Option[(TypeNode, Int)] =
//         Option(a.t, a.dimension)
// }

// Pair Type
case class PairTypeNode(fst: PairElemTypeNode, snd: PairElemTypeNode)
    extends TypeNode {
        val typeId = PairType(fst.typeId, snd.typeId)
}

// Pair Elem Type
sealed trait PairElemTypeNode extends TypeNode

// For the case where just 'pair' is parsed
case class PairElemTypePairNode() extends PairElemTypeNode {
    val typeId = NestedPairType()
}

// Expression
sealed trait ExprNode extends AssignRHSNode

// Unary Operator
sealed trait UnaryOpNode extends ExprNode
case class Not(x: ExprNode) extends UnaryOpNode
case class Neg(x: ExprNode) extends UnaryOpNode
case class Len(x: ExprNode) extends UnaryOpNode
case class Ord(x: ExprNode) extends UnaryOpNode
case class Chr(x: ExprNode) extends UnaryOpNode

// Binary Operator
sealed trait BinaryOpNode extends ExprNode
case class Mult(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class Div(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class Mod(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class Add(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class Sub(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class GT(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class GTE(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class LT(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class LTE(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class Equal(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class NotEqual(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class And(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class Or(x: ExprNode, y: ExprNode) extends BinaryOpNode

// Identifier
case class IdentNode(s: String) extends ExprNode with AssignLHSNode

// Array Elem
case class ArrayElemNode(i: IdentNode, es: List[ExprNode])
    extends ExprNode
    with AssignLHSNode

// Pair Elem
sealed trait PairElemNode extends ExprNode with AssignLHSNode with AssignRHSNode

case class FirstPairElemNode(e: ExprNode) extends PairElemNode

case class SecondPairElemNode(e: ExprNode) extends PairElemNode

// Literals
case class IntLiterNode(i: Int) extends ExprNode

case class BoolLiterNode(b: Boolean) extends ExprNode

case class CharLiterNode(c: Char) extends ExprNode

case class StringLiterNode(s: String) extends ExprNode

case class PairLiterNode() extends ExprNode

case class ArrayLiterNode(es: List[ExprNode]) extends AssignRHSNode
