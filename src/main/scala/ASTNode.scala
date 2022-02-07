sealed trait ASTNode {
    var typeId: Option[Identifier]
    def check(st: SymbolTable): Unit
}
// Program
case class ProgramNode(flist: List[FuncNode], s: StatNode) extends ASTNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

// Function
case class FuncNode(
    t: TypeNode,
    i: IdentNode,
    plist: List[ParamNode],
    s: StatNode
) extends ASTNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

// Param
case class ParamNode(t: TypeNode, i: IdentNode) extends ASTNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

// Statement
sealed trait StatNode extends ASTNode

case class SkipNode() extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

// New variable/array/pair declaration
case class NewAssignNode(t: TypeNode, i: IdentNode, r: AssignRHSNode) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {
        // val varName: String = i.s

        // st.lookup(varName) match {
        //     case None => {
        //         typeId = Some(Variable(t.typeId.get))
        //         st.add(varName, typeId.get)
        //     }
        //     case Some(id) => {
        //         println(varName + " is already declared") // TODO halt semantic checks.
        //     }
        // }
    }
}

case class LRAssignNode(l: AssignLHSNode, r: AssignRHSNode) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {
        
        // l.check(st)
        // r.check(st)

        // l.typeId == r.typeId
    }
}

case class ReadNode(l: AssignLHSNode) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

case class FreeNode(e: ExprNode) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

case class ReturnNode(e: ExprNode) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

case class ExitNode(e: ExprNode) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

case class PrintNode(e: ExprNode) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

case class PrintlnNode(e: ExprNode) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class IfThenElseNode(e: ExprNode, s1: StatNode, s2: StatNode)
    extends StatNode {
        var typeId: Option[Identifier] = None
        def check(st: SymbolTable): Unit = {}
    }

case class WhileDoNode(e: ExprNode, s: StatNode) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

case class BeginEndNode(s: StatNode) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

case class StatListNode(s: List[StatNode]) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

// Assign LHS
sealed trait AssignLHSNode extends ASTNode

// Assign RHS
sealed trait AssignRHSNode extends ASTNode 

case class NewPairNode(e1: ExprNode, e2: ExprNode) extends AssignRHSNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

case class CallNode(i: IdentNode, args: List[ExprNode]) extends AssignRHSNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

// Type
sealed trait TypeNode extends ASTNode

// Base Type
sealed trait BaseTypeNode extends TypeNode with PairElemTypeNode

case class IntTypeNode() extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(IntType())
    def check(st: SymbolTable): Unit = {}
}

case class BoolTypeNode() extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(BoolType())
    def check(st: SymbolTable): Unit = {}
}

case class CharTypeNode() extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(CharType())
    def check(st: SymbolTable): Unit = {}
}

case class StringTypeNode() extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(StringType())
    def check(st: SymbolTable): Unit = {}
}

// Array Type
/* Special Representation: dimension tracks how many dimensions the identifier's
 * array is. This is inserted upon parsing to make it less tedious for
 * semantic checking.
 */
case class ArrayTypeNode(t: TypeNode, dimension: Int = 1)
    extends PairElemTypeNode
    with TypeNode {
        var typeId: Option[Identifier] = None
        def check(st: SymbolTable): Unit = {}
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
        var typeId: Option[Identifier] = None
        def check(st: SymbolTable): Unit = {}
    }

// Pair Elem Type
sealed trait PairElemTypeNode extends TypeNode

// For the case where just 'pair' is parsed
case class PairElemTypePairNode() extends PairElemTypeNode {
    var typeId: Option[Identifier] = Some(NestedPairType())
    def check(st: SymbolTable): Unit = {}
}

// Expression
sealed trait ExprNode extends AssignRHSNode

// Unary Operator
sealed trait UnaryOpNode extends ExprNode
case class Not(x: ExprNode) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class Neg(x: ExprNode) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class Len(x: ExprNode) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class Ord(x: ExprNode) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class Chr(x: ExprNode) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

// Binary Operator
sealed trait BinaryOpNode extends ExprNode
case class Mult(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {
        x.check(st)
        y.check(st)
        x.typeId.get match {
            case IntType() => {
            y.typeId.get match {
                case IntType() => {this.typeId = Some(IntType())}
                case _         => println( "incompatible type")
            }
                }
            case _        => println( "incompatible type")    
        }
    
    }
}
case class Div(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class Mod(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class Add(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class Sub(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class GT(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class GTE(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class LT(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class LTE(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class Equal(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class NotEqual(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class And(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
case class Or(x: ExprNode, y: ExprNode) extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

// Identifier
case class IdentNode(s: String) extends ExprNode with AssignLHSNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

// Array Elem
case class ArrayElemNode(i: IdentNode, es: List[ExprNode])
    extends ExprNode
    with AssignLHSNode {
        var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
    }

// Pair Elem
sealed trait PairElemNode extends ExprNode with AssignLHSNode with AssignRHSNode

case class FirstPairElemNode(e: ExprNode) extends PairElemNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

case class SecondPairElemNode(e: ExprNode) extends PairElemNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}

// Literals
case class IntLiterNode(i: Int) extends ExprNode {
    var typeId: Option[Identifier] = Some(IntType())
    def check(st: SymbolTable): Unit = {}
}

case class BoolLiterNode(b: Boolean) extends ExprNode {
    var typeId: Option[Identifier] =Some(BoolType())
    def check(st: SymbolTable): Unit = {}
}

case class CharLiterNode(c: Char) extends ExprNode {
    var typeId: Option[Identifier] = Some(CharType())
    def check(st: SymbolTable): Unit = {}
}

case class StringLiterNode(s: String) extends ExprNode {
    var typeId: Option[Identifier] = Some(StringType())
    def check(st: SymbolTable): Unit = {}
}

case class PairLiterNode() extends ExprNode {
    var typeId: Option[Identifier] = Some(NullPairType())
    def check(st: SymbolTable): Unit = {}
}

case class ArrayLiterNode(es: List[ExprNode]) extends AssignRHSNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable): Unit = {}
}
