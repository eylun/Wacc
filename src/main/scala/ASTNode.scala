import parsley.Parsley
import parsley.Parsley._
import parsley.implicits.zipped.{LazyZipped2, LazyZipped3, Zipped4}
sealed trait ASTNode {
    val pos: (Int, Int)
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

class SkipNode()(val pos: (Int, Int)) extends StatNode

object SkipNode {
    def apply(): Parsley[SkipNode] = pos.map(p => new SkipNode()(p))
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
        pos <**> (t, i, r).lazyZipped(NewAssignNode(_, _, _) _)
}

case class LRAssignNode(l: AssignLHSNode, r: AssignRHSNode)(val pos: (Int, Int))
    extends StatNode

object LRAssignNode {
    def apply(
        l: => Parsley[AssignLHSNode],
        r: => Parsley[AssignRHSNode]
    ): Parsley[LRAssignNode] =
        pos <**> (l, r).lazyZipped(LRAssignNode(_, _) _)
}

case class ReadNode(l: AssignLHSNode)(val pos: (Int, Int)) extends StatNode

object ReadNode {
    def apply(l: => Parsley[AssignLHSNode]): Parsley[ReadNode] =
        pos <**> l.map(ReadNode(_) _)
}
case class FreeNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode

object FreeNode {
    def apply(e: => Parsley[ExprNode]): Parsley[FreeNode] =
        pos <**> e.map(FreeNode(_) _)
}
case class ReturnNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode

object ReturnNode {
    def apply(e: => Parsley[ExprNode]): Parsley[ReturnNode] =
        pos <**> e.map(ReturnNode(_) _)
}

case class ExitNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode

object ExitNode {
    def apply(e: => Parsley[ExprNode]): Parsley[ExitNode] =
        pos <**> e.map(ExitNode(_) _)
}

case class PrintNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode

object PrintNode {
    def apply(e: => Parsley[ExprNode]): Parsley[PrintNode] =
        pos <**> e.map(PrintNode(_) _)
}

case class PrintlnNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode

object PrintlnNode {
    def apply(e: => Parsley[ExprNode]): Parsley[PrintlnNode] =
        pos <**> e.map(PrintlnNode(_) _)
}
case class IfThenElseNode(e: ExprNode, s1: StatNode, s2: StatNode)(
    val pos: (Int, Int)
) extends StatNode

object IfThenElseNode {
    def apply(
        e: => Parsley[ExprNode],
        s1: => Parsley[StatNode],
        s2: => Parsley[StatNode]
    ): Parsley[IfThenElseNode] =
        pos <**> (e, s1, s2).lazyZipped(IfThenElseNode(_, _, _) _)
}
case class WhileDoNode(e: ExprNode, s: StatNode)(val pos: (Int, Int))
    extends StatNode

object WhileDoNode {
    def apply(
        e: => Parsley[ExprNode],
        s: => Parsley[StatNode]
    ): Parsley[WhileDoNode] =
        pos <**> (e, s).lazyZipped(WhileDoNode(_, _) _)
}

case class BeginEndNode(s: StatNode)(val pos: (Int, Int)) extends StatNode

object BeginEndNode {
    def apply(s: => Parsley[StatNode]): Parsley[BeginEndNode] =
        pos <**> s.map(BeginEndNode(_) _)
}

case class StatListNode(s: List[StatNode])(val pos: (Int, Int)) extends StatNode

object StatListNode {
    def apply(s: => Parsley[List[StatNode]]): Parsley[StatListNode] =
        pos <**> s.map(StatListNode(_) _)
}

// Assign LHS
sealed trait AssignLHSNode extends ASTNode

// Assign RHS
sealed trait AssignRHSNode extends ASTNode

case class NewPairNode(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends AssignRHSNode

object NewPairNode {
    def apply(
        e1: => Parsley[ExprNode],
        e2: => Parsley[ExprNode]
    ): Parsley[NewPairNode] =
        pos <**> (e1, e2).lazyZipped(NewPairNode(_, _) _)
}

case class CallNode(i: IdentNode, args: List[ExprNode])(val pos: (Int, Int))
    extends AssignRHSNode

object CallNode {
    def apply(
        i: => Parsley[IdentNode],
        args: => Parsley[List[ExprNode]]
    ): Parsley[CallNode] =
        pos <**> (i, args).lazyZipped(CallNode(_, _) _)
}

// Type
sealed trait TypeNode extends ASTNode {
    val typeId: Type
}

// Base Type
sealed trait BaseTypeNode extends TypeNode with PairElemTypeNode

class IntTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    val typeId = IntType(Math.pow(2, -31).toInt, Math.pow(2, 31).toInt)
}

object IntTypeNode {
    def apply(): Parsley[IntTypeNode] = pos.map(p => new IntTypeNode()(p))
}

class BoolTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    val typeId = BoolType()
}

object BoolTypeNode {
    def apply(): Parsley[BoolTypeNode] = pos.map(p => new BoolTypeNode()(p))
}

class CharTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    val typeId = CharType()
}

object CharTypeNode {
    def apply(): Parsley[CharTypeNode] = pos.map(p => new CharTypeNode()(p))
}

class StringTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    val typeId = StringType()
}

object StringTypeNode {
    def apply(): Parsley[StringTypeNode] = pos.map(p => new StringTypeNode()(p))
}

// Array Type
/* Special Representation: dimension tracks how many dimensions the identifier's
 * array is. This is inserted upon parsing to make it less tedious for
 * semantic checking.
 */
case class ArrayTypeNode(t: TypeNode, dimension: Int = 1)
    extends PairElemTypeNode
    with TypeNode {
    val pos: (Int, Int) = t.pos
    val typeId = ArrayType(t.typeId, 0)
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
class PairElemTypePairNode()(val pos: (Int, Int)) extends PairElemTypeNode {
    val typeId = NestedPairType()
}

object PairElemTypePairNode {
    def apply(): Parsley[PairElemTypePairNode] =
        pos.map(p => new PairElemTypePairNode()(p))
}

// Expression
sealed trait ExprNode extends AssignRHSNode

// ParserBuilder for operator builder patterns
// implementors of ParserBuilder can serve as a builder of parsers of type T
trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] = parser <* p
}

trait ParserBuilderPos1[T1, R] extends ParserBuilder[T1 => R] {
    def apply(x: T1)(pos: (Int, Int)): R
    val parser = pos.map(p => apply(_)(p))
}

// Unary Operator
sealed trait UnaryOpNode extends ExprNode
case class Not(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode

// Ops(Prefix)(Not <# 'not') ----> returns Parser of type Not
object Not extends ParserBuilderPos1[ExprNode, Not]

case class Neg(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode

object Neg extends ParserBuilderPos1[ExprNode, Neg]

case class Len(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode

object Len extends ParserBuilderPos1[ExprNode, Len]

case class Ord(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode

object Ord extends ParserBuilderPos1[ExprNode, Ord]

case class Chr(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode

object Chr extends ParserBuilderPos1[ExprNode, Chr]

trait ParserBuilderPos2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
    def apply(x: T1, y: T2)(pos: (Int, Int)): R
    val parser = pos.map(p => apply(_, _)(p))
}
// Binary Operator
sealed trait BinaryOpNode extends ExprNode
case class Mult(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object Mult extends ParserBuilderPos2[ExprNode, ExprNode, Mult]

case class Div(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object Div extends ParserBuilderPos2[ExprNode, ExprNode, Div]
case class Mod(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object Mod extends ParserBuilderPos2[ExprNode, ExprNode, Mod]
case class Add(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object Add extends ParserBuilderPos2[ExprNode, ExprNode, Add]

case class Sub(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object Sub extends ParserBuilderPos2[ExprNode, ExprNode, Sub]

case class GT(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object GT extends ParserBuilderPos2[ExprNode, ExprNode, GT]

case class GTE(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object GTE extends ParserBuilderPos2[ExprNode, ExprNode, GTE]

case class LT(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object LT extends ParserBuilderPos2[ExprNode, ExprNode, LT]

case class LTE(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object LTE extends ParserBuilderPos2[ExprNode, ExprNode, LTE]

case class Equal(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object Equal extends ParserBuilderPos2[ExprNode, ExprNode, Equal]

case class NotEqual(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object NotEqual extends ParserBuilderPos2[ExprNode, ExprNode, NotEqual]

case class And(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object And extends ParserBuilderPos2[ExprNode, ExprNode, And]

case class Or(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode

object Or extends ParserBuilderPos2[ExprNode, ExprNode, Or]

// Identifier
case class IdentNode(s: String)(val pos: (Int, Int))
    extends ExprNode
    with AssignLHSNode

object IdentNode {
    def apply(s: => Parsley[String]): Parsley[IdentNode] =
        pos <**> s.map(IdentNode(_) _)
}

// Array Elem
case class ArrayElemNode(i: IdentNode, es: List[ExprNode])(val pos: (Int, Int))
    extends ExprNode
    with AssignLHSNode

object ArrayElemNode {
    def apply(
        i: => Parsley[IdentNode],
        es: Parsley[List[ExprNode]]
    ): Parsley[ArrayElemNode] =
        pos <**> (i, es).lazyZipped(ArrayElemNode(_, _) _)
}

// Pair Elem
sealed trait PairElemNode extends ExprNode with AssignLHSNode with AssignRHSNode

case class FirstPairElemNode(e: ExprNode)(val pos: (Int, Int))
    extends PairElemNode

object FirstPairElemNode {
    def apply(e: => Parsley[ExprNode]): Parsley[FirstPairElemNode] =
        pos <**> e.map(FirstPairElemNode(_) _)
}

case class SecondPairElemNode(e: ExprNode)(val pos: (Int, Int))
    extends PairElemNode

object SecondPairElemNode {
    def apply(e: => Parsley[ExprNode]): Parsley[SecondPairElemNode] =
        pos <**> e.map(SecondPairElemNode(_) _)
}

// Literals
case class IntLiterNode(i: Int)(val pos: (Int, Int)) extends ExprNode

object IntLiterNode {
    def apply(i: => Parsley[Int]): Parsley[IntLiterNode] =
        pos <**> i.map(IntLiterNode(_) _)
}

case class BoolLiterNode(b: Boolean)(val pos: (Int, Int)) extends ExprNode

object BoolLiterNode {
    def apply(b: => Parsley[Boolean]): Parsley[BoolLiterNode] =
        pos <**> b.map(BoolLiterNode(_) _)
}

case class CharLiterNode(c: Char)(val pos: (Int, Int)) extends ExprNode

object CharLiterNode {
    def apply(c: => Parsley[Char]): Parsley[CharLiterNode] =
        pos <**> c.map(CharLiterNode(_) _)
}

case class StringLiterNode(s: String)(val pos: (Int, Int)) extends ExprNode

object StringLiterNode {
    def apply(s: => Parsley[String]): Parsley[StringLiterNode] =
        pos <**> s.map(StringLiterNode(_) _)
}

class PairLiterNode()(val pos: (Int, Int)) extends ExprNode

object PairLiterNode {
    def apply(): Parsley[PairLiterNode] = pos.map(p => new PairLiterNode()(p))
}

case class ArrayLiterNode(es: List[ExprNode])(val pos: (Int, Int))
    extends AssignRHSNode

object ArrayLiterNode {
    def apply(es: => Parsley[List[ExprNode]]): Parsley[ArrayLiterNode] =
        pos <**> es.map(ArrayLiterNode(_) _)
}
