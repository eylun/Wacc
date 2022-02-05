sealed trait ASTNode

// Program
case class ProgramNode(flist: List[FuncNode], s: StatNode) extends ASTNode

// Function
case class FuncNode(t: TypeNode, i: IdentNode, plist: List[ParamNode])
    extends ASTNode

// Param
case class ParamNode(t: TypeNode, i: IdentNode) extends ASTNode

// Statement
sealed trait StatNode extends ASTNode

case class SkipNode() extends StatNode

case class NewAssignNode(t: TypeNode, i: IdentNode) extends StatNode

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

// Assign LHS
sealed trait AssignLHSNode extends ASTNode

// Assign RHS
sealed trait AssignRHSNode extends ASTNode

case class NewPairNode(e1: ExprNode, e2: ExprNode) extends AssignRHSNode

case class CallNode(i: IdentNode, args: List[ExprNode]) extends AssignRHSNode

// Type
sealed trait TypeNode extends ASTNode

// Base Type
sealed trait BaseTypeNode extends TypeNode with PairElemTypeNode

case class IntTypeNode() extends BaseTypeNode

case class BoolTypeNode() extends BaseTypeNode

case class CharTypeNode() extends BaseTypeNode

case class StringTypeNode() extends BaseTypeNode

// Array Type
/* Special Representation: dimension tracks how many dimensions the identifier's
 * array is. This is inserted upon parsing to make it less tedious for
 * semantic checking.
 */
case class ArrayTypeNode(t: TypeNode, dimension: Int)
    extends TypeNode
    with PairElemTypeNode

// Pair Type
case class PairTypeNode(fst: PairElemTypeNode, snd: PairElemTypeNode)
    extends TypeNode

// Pair Elem Type
sealed trait PairElemTypeNode extends ASTNode

// For the case where just 'pair' is parsed
case class PairElemTypePairNode() extends PairElemTypeNode

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

// Indentifier
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
