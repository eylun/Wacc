// Confine ASTNode within this file only, it should never be called outside
sealed trait ASTNode

// Program
case class ProgramNode(flist: List[FuncNode], s: StatNode) extends ASTNode {}

// Function
case class FuncNode(t: TypeNode) extends ASTNode {}

// Param
case class ParamNode() extends ASTNode {}

// Statement
sealed trait StatNode extends ASTNode

// Assign LHS
sealed trait AssignLHSNode extends ASTNode

// Assign RHS
sealed trait AssignRHSNode extends ASTNode

// Argument List
case class ArgListNode() extends ASTNode {}

// Pair Elem
sealed trait PairElemNode extends ASTNode

// Type
sealed trait TypeNode extends ASTNode

// Base Type
sealed trait BaseTypeNode extends ASTNode

// Pair Elem Type
sealed trait PairElemTypeNode extends ASTNode

// Expression
sealed trait ExprNode extends ASTNode
case class 


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
