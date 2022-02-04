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
case class ArrayTypeNode() extends BaseTypeNode with PairElemTypeNode

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
case class Greater(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class GreaterOrEqual(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class Smaller(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class SmallerOrEqual(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class Equal(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class NotEqual(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class Equal(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class NotEqual(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class And(x: ExprNode, y: ExprNode) extends BinaryOpNode
case class Or(x: ExprNode, y: ExprNode) extends BinaryOpNode

// Indentifier
case class IdentNode() extends ExprNode with AssignLHSNode

// Array Elem
case class ArrayElemNode() extends ExprNode with AssignLHSNode

// Pair Elem
sealed trait PairElemNode extends ExprNode with AssignLHSNode with AssignRHSNode

case class FirstPairElemNode(e: ExprNode) extends PairElemNode

case class SecondPairElemNode(e: ExprNode) extends PairElemNode

// Literals
case class IntLiter() extends ExprNode

case class BoolLiter() extends ExprNode

case class CharLiter() extends ExprNode

case class StrLiter() extends ExprNode

case class PairLiter() extends ExprNode

case class ArrayLiterNode() extends ExprNode with AssignRHSNode
