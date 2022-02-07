sealed trait ASTNode {
    val pos: (Int, Int)
    import parsley.Parsley.pos
    import parsley.implicits.zipped.LazyZipped2
}

// Program
case class ProgramNode(flist: List[FuncNode], s: StatNode)(val pos: (Int, Int))
    extends ASTNode

object ProgramNode {
    def apply(
        flist: => Parsley[List[FuncNode]],
        s: => Parsley[StatNode]
    ): Parsley[ProgramNode] =
        pos <**> (flist, s).lazyZipped(ProgramNode(_, _) _)
}

// Function
case class FuncNode(
    t: TypeNode,
    i: IdentNode,
    plist: List[ParamNode],
    s: StatNode
)(val pos: (Int, Int))
    extends ASTNode

object FuncNode {
    def apply(
        t: => Parsley[TypeNode],
        i: => Parsley[IdentNode],
        plist: => Parsley[List[ParamNode]],
        s: => Parsley[StatNode]
    ): Parsley[FuncNode] =
        pos <**> (t, i, plist, s).zipped(FuncNode(_, _, _, _) _)
}

// Param
case class ParamNode(t: TypeNode, i: IdentNode)(val pos: (Int, Int))
    extends ASTNode

object ParamNode {
    def apply(
        t: => Parsley[TypeNode],
        i: => Parsley[IdentNode]
    ): Parsley[ParamNode] =
        pos <**> (t, i).lazyZipped(ParamNode(_, _) _)
}

// Statement
sealed trait StatNode extends ASTNode

case class SkipNode()(val pos: (Int, Int)) extends StatNode

object SkipNode {
    def apply(): Parsley[SkipNode] = pos <**> pure(SkipNode())
}

case class NewAssignNode(t: TypeNode, i: IdentNode, r: AssignRHSNode)(
    val pos: (Int, Int)
) extends StatNode {
    def check(st: SymbolTable): Unit = {
        val varName: String = i.s
        var varObj: Option[Variable] = None

        st.lookup(varName) match {
            case None => {
                varObj = Some(Variable(t.typeId))
                st.add(varName, varObj.get)
            }
            case Some(id) => {
                println(
                  varName + " is already declared"
                ) // TODO halt semantic checks.
            }
        }
    }
}

object NewAssignNode {
    def apply(
        t: => Parsley[TypeNode],
        i: => Parsley[IdentNode],
        r: => Parsley[AssignRHSNode]
    ): Parsley[NewAssignNode] =
        pos <**> (t, i, r).lazyZipped3(NewAssignNode(_, _, _) _)
}

case class LRAssignNode(l: AssignLHSNode, r: AssignRHSNode)(val pos: (Int, Int))
    extends StatNode

object LRAssignNode {
    def apply(
        l: => Parsley[AssignLHSNode],
        r: => Parsley[AssignRHSNode]
    ): Parsley[LRAssignNode] =
        pos <**> (l, r).lazyZipped2(LRAssignNode(_, _) _)
}

case class ReadNode(l: AssignLHSNode)(val pos: (Int, Int)) extends StatNode

object ReadNode {
    def apply(l: => Parsley[AssignLHSNode]): Parsley[ReadNode] =
        pos <**> l.map(ReadNode(_) _)
}
case class FreeNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode

object FreeNode {
    def apply(e: => Parsley[ExprNode]): Parsley[FreeNode] =
        pos <**> e.map(FreeNode(_), _)
}
case class ReturnNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode

case class ExitNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode

case class PrintNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode

case class PrintlnNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode

case class IfThenElseNode(e: ExprNode, s1: StatNode, s2: StatNode)(
    val pos: (Int, Int)
) extends StatNode

case class WhileDoNode(e: ExprNode, s: StatNode)(val pos: (Int, Int))
    extends StatNode

case class BeginEndNode(s: StatNode)(val pos: (Int, Int)) extends StatNode

case class StatListNode(s: List[StatNode])(val pos: (Int, Int)) extends StatNode

// Assign LHS
sealed trait AssignLHSNode extends ASTNode

// Assign RHS
sealed trait AssignRHSNode extends ASTNode

case class NewPairNode(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends AssignRHSNode

case class CallNode(i: IdentNode, args: List[ExprNode])(val pos: (Int, Int))
    extends AssignRHSNode

// Type
sealed trait TypeNode extends ASTNode {
    val typeId: Type
}

// Base Type
sealed trait BaseTypeNode extends TypeNode with PairElemTypeNode

case class IntTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    val typeId = IntType(Math.pow(2, -31).toInt, Math.pow(2, 31).toInt)
}

object IntTypeNode {
    def apply(): Parsley[IntTypeNode] = pos <**> pure(IntTypeNode())
}

case class BoolTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    val typeId = BoolType()
}

object BoolTypeNode {
    def apply(): Parsley[BoolTypeNode] = pos <**> pure(BoolTypeNode())
}

case class CharTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    val typeId = CharType()
}

object CharTypeNode {
    def apply(): Parsley[CharTypeNode] = pos <**> pure(CharTypeNode())
}

case class StringTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    val typeId = StringType()
}

object StringTypeNode {
    def apply(): Parsley[StringTypeNode] = pos <**> pure(StringTypeNode())
}

// Array Type
/* Special Representation: dimension tracks how many dimensions the identifier's
 * array is. This is inserted upon parsing to make it less tedious for
 * semantic checking.
 */
case class ArrayTypeNode(t: TypeNode, dimension: Int = 1)(val pos: (Int, Int))
    extends PairElemTypeNode
    with TypeNode {
    val typeId = ArrayType(t.typeId, 0)
}

object ArrayTypeNode {
    def apply(
        t: Parsley[TypeNode],
        dimension: Parsley[Int]
    ): Parsley[ArrayTypeNode] =
        pos <**> (t, dimension).lazyZipped(ArrayTypeNode(_, _) _)

}

// Pair Type
case class PairTypeNode(fst: PairElemTypeNode, snd: PairElemTypeNode)(
    val pos: (Int, Int)
) extends TypeNode {
    val typeId = PairType(fst.typeId, snd.typeId)
}

object PairTypeNode {
    def apply(
        fst: => Parsley[PairElemTypeNode],
        snd: Parsley[PairElemTypeNode]
    ): Parsley[PairTypeNode] =
        pos <**> (fst, snd).lazyZipped(PairTypeNode(_, _) _)
}

// Pair Elem Type
sealed trait PairElemTypeNode extends TypeNode

// For the case where just 'pair' is parsed
case class PairElemTypePairNode()(val pos: (Int, Int))
    extends PairElemTypeNode {
    val typeId = NestedPairType()
}

object PairElemTypePairNode {
    def apply(): Parsley[PairElemTypePairNode] =
        pos <**> pure(PairElemTypeNode())
}

// Expression
sealed trait ExprNode extends AssignRHSNode

// Unary Operator
sealed trait UnaryOpNode extends ExprNode
case class Not(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode
case class Neg(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode
case class Len(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode
case class Ord(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode
case class Chr(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode

// Binary Operator
sealed trait BinaryOpNode extends ExprNode
case class Mult(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class Div(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class Mod(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class Add(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class Sub(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class GT(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class GTE(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class LT(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class LTE(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class Equal(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class NotEqual(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class And(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode
case class Or(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

// Identifier
case class IdentNode(s: String)(val pos: (Int, Int))
    extends ExprNode
    with AssignLHSNode

// Array Elem
case class ArrayElemNode(i: IdentNode, es: List[ExprNode])(val pos: (Int, Int))
    extends ExprNode
    with AssignLHSNode

// Pair Elem
sealed trait PairElemNode extends ExprNode with AssignLHSNode with AssignRHSNode

case class FirstPairElemNode(e: ExprNode)(val pos: (Int, Int))
    extends PairElemNode

case class SecondPairElemNode(e: ExprNode)(val pos: (Int, Int))
    extends PairElemNode

// Literals
case class IntLiterNode(i: Int)(val pos: (Int, Int)) extends ExprNode

case class BoolLiterNode(b: Boolean)(val pos: (Int, Int)) extends ExprNode

case class CharLiterNode(c: Char)(val pos: (Int, Int)) extends ExprNode

case class StringLiterNode(s: String)(val pos: (Int, Int)) extends ExprNode

case class PairLiterNode()(val pos: (Int, Int)) extends ExprNode

case class ArrayLiterNode(es: List[ExprNode])(val pos: (Int, Int))
    extends AssignRHSNode
