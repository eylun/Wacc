// Confine ASTNode within this file only, it should never be called outside
sealed trait ASTNode

// Program
case class ProgramNode(flist: List[FuncNode], s: StatNode) extends ASTNode {}

// Function
case class FuncNode(t: TypeNode, ) extends ASTNode {}

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

// Unary Operator
sealed trait UnaryOpNode extends ASTNode

// Binary Operator
sealed trait BinaryOpNode extends ASTNode

// Indentifier
