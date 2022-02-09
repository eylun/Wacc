import parsley.Parsley
import parsley.Parsley._
import parsley.implicits.zipped.{LazyZipped2, LazyZipped3, Zipped4}
import scala.collection.mutable.Map
import semantics.{
    typeCheckArithmeticBinOp,
    typeCheckOrderingBinOp,
    typeCheckEqualityBinOp,
    typeCheckLogicalBinOp
}
import parsley.errors.combinator.ErrorMethods
import java.util.function.UnaryOperator

sealed trait ASTNode {
    var typeId: Option[Identifier]
    def check(st: SymbolTable, errors: List[String]): Unit
}
// Program
case class ProgramNode(flist: List[FuncNode], s: StatNode)(val pos: (Int, Int))
    extends ASTNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        flist.foreach { f => f.check(st, errors) }
        s.check(st, errors)
    }
}

object ProgramNode {
    def apply(
        flist: => Parsley[List[FuncNode]],
        s: => Parsley[StatNode]
    ): Parsley[ProgramNode] =
        pos <**> (flist, s.label("program statements"))
            .lazyZipped(ProgramNode(_, _) _)
}
// Function
case class FuncNode(
    t: TypeNode,
    i: IdentNode,
    plist: List[ParamNode],
    s: StatNode
)(val pos: (Int, Int))
    extends ASTNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        // Check function name does not already exist
        st.lookup(i.s) match {
            case Some(id) => errors :+ (i.s + " is already declared")
            case None => {
                t.check(st, errors)

                // Create new symbol table and link with outer scope
                val funcST = SymbolTable(st)
                // Set type id as FunctionId
                this.typeId = Some(
                  FunctionId(
                    t.typeId.get.getType(),
                    plist.map(p => p.typeId.get.asInstanceOf[Param]).toArray,
                    funcST
                  )
                )

                // Add function name to outer symbol table. DO this first
                // due to possible recursion
                st.add(i.s, this.typeId.get)

                funcST.add("return", t.typeId.get.getType())

                // Check list of params (using new symbol table funcST)
                plist.foreach { p => p.check(funcST, errors) }

            }
        }
    }
}

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
    extends ASTNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        st.lookup(i.s) match {
            case Some(id) => errors :+ (i.s + " is already declared")
            case None => {
                t.check(st, errors)
                st.add(i.s, Param(t.typeId.get.getType()))
                this.typeId = t.typeId
            }
        }
    }
}

object ParamNode {
    def apply(
        t: => Parsley[TypeNode],
        i: => Parsley[IdentNode]
    ): Parsley[ParamNode] =
        pos <**> (t, i).lazyZipped(ParamNode(_, _) _)
}

// Statement
sealed trait StatNode extends ASTNode

case class SkipNode()(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}

object SkipNode extends ParserBuilderPos0[SkipNode]

case class NewAssignNode(t: TypeNode, i: IdentNode, r: AssignRHSNode)(
    val pos: (Int, Int)
) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        t.check(st, errors)
        r.check(st, errors)
        if (r.typeId.get.getType() != t.typeId.get.getType()) {
            errors :+ "variable assignment has incompatible type"
        }
        st.lookup(i.s) match {
            case None => {
                st.add(i.s, Variable(t.typeId.get.getType()))

            }
            case Some(FunctionId(_, _, _)) => {
                /* When a function node already exists, manually rename the
                 * identifer in the AST node and insert into the symbol table */
                st.lookup(i.s + "$") match {
                    case None => {
                        i.s += "$"
                        st.add(i.s, Variable(t.typeId.get.getType()))
                    }
                    case Some(_) =>
                        errors :+ s" ${i.s} is already declared within the scope"
                }
            }
            case _ =>
                errors :+ s" ${i.s} is already declared within the scope"
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
    extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        l.check(st, errors)
        r.check(st, errors)
        if (l.typeId.get.getType() != r.typeId.get.getType())
            errors :+
                s"lhs is type ${l.typeId.get.getType()}, rhs is type ${r.typeId.get.getType()}"
    }
}

object LRAssignNode {
    def apply(
        l: => Parsley[AssignLHSNode],
        r: => Parsley[AssignRHSNode]
    ): Parsley[LRAssignNode] =
        pos <**> (l, r).lazyZipped(LRAssignNode(_, _) _)
}

case class ReadNode(l: AssignLHSNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        l.check(st, errors)
    }
}

object ReadNode {
    def apply(l: => Parsley[AssignLHSNode]): Parsley[ReadNode] =
        pos <**> l.map(ReadNode(_) _)
}
case class FreeNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        e.check(st, errors)
        e.typeId.get match {
            case ArrayType(_, _) | PairType(_, _) => {}
            case _ =>
                errors :+
                    "expression is not a valid reference to a pair or array, could not be freed."

        }
    }
}

object FreeNode {
    def apply(e: => Parsley[ExprNode]): Parsley[FreeNode] =
        pos <**> e.map(FreeNode(_) _)
}
case class ReturnNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        e.check(st, errors)

        st.lookup("return") match {
            case Some(id) => {
                if (e.typeId.get.getType() != id.getType()) {
                    errors :+
                        s"function is of type ${id.getType()} but tries to return type ${e.typeId.get.getType()} "
                }
            }
            case None =>
                errors :+ "return statement called outside of a function scope"
        }
    }
}

object ReturnNode {
    def apply(e: => Parsley[ExprNode]): Parsley[ReturnNode] =
        pos <**> e.map(ReturnNode(_) _)
}

case class ExitNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        e.check(st, errors)
        e.typeId.get match {
            case IntType() | Variable(IntType()) => {
                this.typeId = Some(IntType())
            }
            case _ =>
                errors :+
                    "incompatible type: 'exit' takes an expression of type 'int'"
        }
    }
}

object ExitNode {
    def apply(e: => Parsley[ExprNode]): Parsley[ExitNode] =
        pos <**> e.map(ExitNode(_) _)
}

case class PrintNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        e.check(st, errors)
    }
}

object PrintNode {
    def apply(e: => Parsley[ExprNode]): Parsley[PrintNode] =
        pos <**> e.map(PrintNode(_) _)
}

/* println can print any literal or arrays. It CANNOT print functions
 * Expr can be any literal or identifier, which can be a function */
case class PrintlnNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        e.check(st, errors)
        e.typeId.get match {
            case (FunctionId(_, _, _)) => {
                errors :+ "incompatible value: functions cannot be printed"
            }
            case _ => {}
        }
    }
}

object PrintlnNode {
    def apply(e: => Parsley[ExprNode]): Parsley[PrintlnNode] =
        pos <**> e.map(PrintlnNode(_) _)
}
case class IfThenElseNode(e: ExprNode, s1: StatNode, s2: StatNode)(
    val pos: (Int, Int)
) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        e.check(st, errors)
        e.typeId.get match {
            case BoolType() | Variable(BoolType()) => {
                s1.check(st, errors)
                s2.check(st, errors)
            }
            case _ =>
                errors :+ "incompatible type for conditional: 'if' condition must be a boolean"
        }
    }
}

object IfThenElseNode {
    def apply(
        e: => Parsley[ExprNode],
        s1: => Parsley[StatNode],
        s2: => Parsley[StatNode]
    ): Parsley[IfThenElseNode] =
        pos <**> (e, s1, s2).lazyZipped(IfThenElseNode(_, _, _) _)
}
case class WhileDoNode(e: ExprNode, s: StatNode)(val pos: (Int, Int))
    extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        e.check(st, errors)
        e.typeId.get match {
            case BoolType() | Variable(BoolType()) => {
                s.check(st, errors)
            }
            case _ =>
                errors :+ "incompatible type for while loop: 'while' condition must be a boolean"
        }
    }
}

object WhileDoNode {
    def apply(
        e: => Parsley[ExprNode],
        s: => Parsley[StatNode]
    ): Parsley[WhileDoNode] =
        pos <**> (e, s).lazyZipped(WhileDoNode(_, _) _)
}

case class BeginEndNode(s: StatNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        // Create new symbol table and link with outer scope
        val newScopeST = SymbolTable(st)
        s.check(newScopeST, errors)
    }
}

object BeginEndNode {
    def apply(s: => Parsley[StatNode]): Parsley[BeginEndNode] =
        pos <**> s.map(BeginEndNode(_) _)
}

case class StatListNode(s: List[StatNode])(val pos: (Int, Int))
    extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        s.foreach { stat => stat.check(st, errors) }
    }
}
object StatListNode {
    def apply(s: => Parsley[List[StatNode]]): Parsley[StatListNode] =
        pos <**> s.map(StatListNode(_) _)
}

// Assign LHS
sealed trait AssignLHSNode extends ASTNode

// Assign RHS
sealed trait AssignRHSNode extends ASTNode

case class NewPairNode(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends AssignRHSNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        e1.check(st, errors)
        e2.check(st, errors)
        this.typeId = Some(
          PairType(
            e1.typeId.get.getType(),
            e2.typeId.get.getType()
          )
        )
    }
}

object NewPairNode {
    def apply(
        e1: => Parsley[ExprNode],
        e2: => Parsley[ExprNode]
    ): Parsley[NewPairNode] =
        pos <**> (e1, e2).lazyZipped(NewPairNode(_, _) _)
}

case class CallNode(i: IdentNode, args: List[ExprNode])(val pos: (Int, Int))
    extends AssignRHSNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        st.lookupAll(i.s) match {
            case Some(FunctionId(returnType, params, funcST)) => {
                args.foreach { arg => arg.check(st, errors) }
                val paramTypes = params.map { p =>
                    p match {
                        case Param(t) => t
                    }
                }

                if (args.length < paramTypes.length)
                    errors :+ "insufficient arguments provided"

                // Compare arg types against param list
                for ((arg, index) <- args.zipWithIndex) {
                    if (index < paramTypes.length) {
                        arg.typeId.get match {
                            case Variable(t) => {
                                if (t != paramTypes(index)) {
                                    errors :+ "argument type does not match that of the parameter"
                                }
                            }
                            case _ => {
                                if (arg.typeId.get != paramTypes(index)) {
                                    errors :+ "argument type does not match that of the parameter"
                                }
                            }
                        }
                    } else {
                        errors :+ "too many arguments provided"
                    }
                }
                this.typeId = Some(returnType)
            }
            case _ => errors :+ "function not defined"
        }
    }
}

object CallNode {
    def apply(
        i: => Parsley[IdentNode],
        args: => Parsley[List[ExprNode]]
    ): Parsley[CallNode] =
        pos <**> (i, args).lazyZipped(CallNode(_, _) _)
}

// Type
sealed trait TypeNode extends ASTNode

// Base Type
sealed trait BaseTypeNode extends TypeNode with PairElemTypeNode

case class IntTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(IntType())
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}

object IntTypeNode extends ParserBuilderPos0[IntTypeNode]

case class BoolTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(BoolType())
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}

object BoolTypeNode extends ParserBuilderPos0[BoolTypeNode]

case class CharTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(CharType())
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}
object CharTypeNode extends ParserBuilderPos0[CharTypeNode]

case class StringTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(StringType())
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}

object StringTypeNode extends ParserBuilderPos0[StringTypeNode]

// Pair Type
case class PairTypeNode(fst: PairElemTypeNode, snd: PairElemTypeNode)(
    val pos: (Int, Int)
) extends TypeNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        fst.check(st, errors)
        snd.check(st, errors)
        this.typeId = Some(
          PairType(
            fst.typeId.get.getType(),
            snd.typeId.get.getType()
          )
        )
    }
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
    var typeId: Option[Identifier] = Some(NestedPairType())
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}

object PairElemTypePairNode extends ParserBuilderPos0[PairElemTypePairNode]

// Expression
sealed trait ExprNode extends AssignRHSNode

// ParserBuilder for operator builder patterns
// implementors of ParserBuilder can serve as a builder of parsers of type T
trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] = parser <* p.label("operator")
}

trait ParserBuilderPos0[R] extends ParserBuilder[R] {
    def apply()(pos: (Int, Int)): R
    val parser = pos.map(p => apply()(p))
}

trait ParserBuilderPos1[T1, R] extends ParserBuilder[T1 => R] {
    def apply(x: T1)(pos: (Int, Int)): R
    val parser = pos.map(p => (apply(_)(p)))
}

// Unary Operator
sealed trait UnaryOpNode extends ExprNode

// Array Type
/* Special Representation: dimension tracks how many dimensions the identifier's
 * array is. This is inserted upon parsing to make it less tedious for
 * semantic checking.
 */
case class ArrayTypeNode(t: TypeNode, dimension: Int)(pos: (Int, Int))
    extends PairElemTypeNode
    with UnaryOpNode
    with TypeNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        t.check(st, errors)
        this.typeId = Some(
          ArrayType(t.typeId.get.getType(), dimension)
        )
    }
}
object ArrayTypeNode {
    // Apply function for chain operation
    def apply(op: => Parsley[Unit]): Parsley[ArrayTypeNode => ArrayTypeNode] =
        pos.map[ArrayTypeNode => ArrayTypeNode](p =>
            ((at: ArrayTypeNode) => at.copy(dimension = at.dimension + 1)(p))
        ) <* op

    // Apply function for first creation
    def apply(
        t: => Parsley[TypeNode],
        dimension: => Int
    ): Parsley[ArrayTypeNode] =
        pos <**> t.map(ArrayTypeNode(_, dimension) _)
}

case class Not(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        x.check(st, errors)
        x.typeId.get match {
            case BoolType() | Variable(BoolType()) =>
                this.typeId = Some(BoolType())
            case _ => errors :+ "incompatible argument type for operator 'not'"
        }
    }
}

// Ops(Prefix)(Not <# 'not') ----> returns Parser of type Not
object Not extends ParserBuilderPos1[ExprNode, Not]

case class Neg(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        x.check(st, errors)
        x.typeId.get match {
            case IntType() | Variable(IntType()) =>
                this.typeId = Some(IntType())
            case _ => errors :+ "incompatible argument type for operator 'neg'"
        }
    }
}

object Neg extends ParserBuilderPos1[ExprNode, Neg]

case class Len(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        x.check(st, errors)
        x.typeId.get match {
            case ArrayType(_, _) | Variable(ArrayType(_, _)) =>
                this.typeId = Some(IntType())
            case _ => errors :+ "incompatible argument type for operator 'len'"
        }
    }
}

object Len extends ParserBuilderPos1[ExprNode, Len]

case class Ord(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    // add list of wacc errors as parameter later
    def check(st: SymbolTable, errors: List[String]): Unit = {
        x.check(st, errors)
        x.typeId.get match {
            case CharType() | Variable(CharType()) =>
                this.typeId = Some(IntType())
            case _ => errors :+ "incompatible argument type for operator 'ord'"
        }
    }
}

object Ord extends ParserBuilderPos1[ExprNode, Ord]

case class Chr(x: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        x.check(st, errors)
        x.typeId.get match {
            case IntType() | Variable(IntType()) =>
                this.typeId = Some(CharType())
            case _ => errors :+ "incompatible argument type for operator 'chr'"
        }
    }
}

object Chr extends ParserBuilderPos1[ExprNode, Chr]

trait ParserBuilderPos2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
    def apply(x: T1, y: T2)(pos: (Int, Int)): R
    val parser = pos.map(p => apply(_, _)(p))
}
// Binary Operator
sealed trait BinaryOpNode extends ExprNode
case class Mult(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckArithmeticBinOp(st, this, x, y, errors)
    }
}

object Mult extends ParserBuilderPos2[ExprNode, ExprNode, Mult]

case class Div(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckArithmeticBinOp(st, this, x, y, errors)
    }
}

object Div extends ParserBuilderPos2[ExprNode, ExprNode, Div]
case class Mod(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckArithmeticBinOp(st, this, x, y, errors)
    }
}

object Mod extends ParserBuilderPos2[ExprNode, ExprNode, Mod]
case class Add(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckArithmeticBinOp(st, this, x, y, errors)
    }
}

object Add extends ParserBuilderPos2[ExprNode, ExprNode, Add]

case class Sub(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckArithmeticBinOp(st, this, x, y, errors)
    }
}

object Sub extends ParserBuilderPos2[ExprNode, ExprNode, Sub]

case class GT(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckOrderingBinOp(st, this, x, y, errors)
    }
}

object GT extends ParserBuilderPos2[ExprNode, ExprNode, GT]

case class GTE(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckOrderingBinOp(st, this, x, y, errors)
    }
}

object GTE extends ParserBuilderPos2[ExprNode, ExprNode, GTE]

case class LT(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckOrderingBinOp(st, this, x, y, errors)
    }
}

object LT extends ParserBuilderPos2[ExprNode, ExprNode, LT]

case class LTE(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckOrderingBinOp(st, this, x, y, errors)
    }
}

object LTE extends ParserBuilderPos2[ExprNode, ExprNode, LTE]

case class Equal(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckEqualityBinOp(st, this, x, y, errors)
    }
}

object Equal extends ParserBuilderPos2[ExprNode, ExprNode, Equal]

case class NotEqual(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckEqualityBinOp(st, this, x, y, errors)
    }
}

object NotEqual extends ParserBuilderPos2[ExprNode, ExprNode, NotEqual]

case class And(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckLogicalBinOp(st, this, x, y, errors)
    }
}

object And extends ParserBuilderPos2[ExprNode, ExprNode, And]

case class Or(x: ExprNode, y: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        typeCheckLogicalBinOp(st, this, x, y, errors)
    }
}

object Or extends ParserBuilderPos2[ExprNode, ExprNode, Or]

// Identifier
case class IdentNode(var s: String)(val pos: (Int, Int))
    extends ExprNode
    with AssignLHSNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {

        st.lookup(s) match {
            case None    => errors :+ "identifier does not exist in scope"
            case Some(FunctionId(_, _, _)) => {
                st.lookup(s + "$") match {
                    case None    => errors :+ "identifier does not exist in scope"
                    case i @ Some(_)  => this.typeId = i
                }
            }
            case i @ Some(_) => this.typeId = i
        }
    }
}

object IdentNode {
    def apply(s: => Parsley[String]): Parsley[IdentNode] =
        pos <**> s.map(IdentNode(_) _)
}

// Array Elem
case class ArrayElemNode(i: IdentNode, es: List[ExprNode])(val pos: (Int, Int))
    extends ExprNode
    with AssignLHSNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        // i.check(st, errors)
        // i.typeId.get match {
        //     case ArrayType(t, d) => 
        // }
    }
}

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
    extends PairElemNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        e.check(st, errors)
        e.typeId.get match {
            case Variable(PairType(t,_)) => this.typeId = Some(t)
            case PairType(t,_)           => this.typeId = Some(t)
            case _                       => errors :+ "expression is not of type pair"
        }
    }
}

object FirstPairElemNode {
    def apply(e: => Parsley[ExprNode]): Parsley[FirstPairElemNode] =
        pos <**> e.map(FirstPairElemNode(_) _)
}

case class SecondPairElemNode(e: ExprNode)(val pos: (Int, Int))
    extends PairElemNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        e.check(st, errors)
        e.typeId.get match {
            case Variable(PairType(_,t)) => this.typeId = Some(t)
            case PairType(_,t)           => this.typeId = Some(t)
            case _                       => errors :+ "expression is not of type pair"
        }
    }
}

object SecondPairElemNode {
    def apply(e: => Parsley[ExprNode]): Parsley[SecondPairElemNode] =
        pos <**> e.map(SecondPairElemNode(_) _)
}

// Literals
case class IntLiterNode(i: Int)(val pos: (Int, Int)) extends ExprNode {
    var typeId: Option[Identifier] = Some(IntType())
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}

object IntLiterNode {
    def apply(i: => Parsley[Int]): Parsley[IntLiterNode] =
        pos <**> i.map(IntLiterNode(_) _)
}

case class BoolLiterNode(b: Boolean)(val pos: (Int, Int)) extends ExprNode {
    var typeId: Option[Identifier] = Some(BoolType())
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}

object BoolLiterNode {
    def apply(b: => Parsley[Boolean]): Parsley[BoolLiterNode] =
        pos <**> b.map(BoolLiterNode(_) _)
}

case class CharLiterNode(c: Char)(val pos: (Int, Int)) extends ExprNode {
    var typeId: Option[Identifier] = Some(CharType())
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}

object CharLiterNode {
    def apply(c: => Parsley[Char]): Parsley[CharLiterNode] =
        pos <**> c.map(CharLiterNode(_) _)
}

case class StringLiterNode(s: String)(val pos: (Int, Int)) extends ExprNode {
    var typeId: Option[Identifier] = Some(StringType())
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}

object StringLiterNode {
    def apply(s: => Parsley[String]): Parsley[StringLiterNode] =
        pos <**> s.map(StringLiterNode(_) _)
}

class PairLiterNode()(val pos: (Int, Int)) extends ExprNode {
    var typeId: Option[Identifier] = Some(NullPairType())
    def check(st: SymbolTable, errors: List[String]): Unit = {}
}

object PairLiterNode {
    def apply(): Parsley[PairLiterNode] = pos.map(p => new PairLiterNode()(p))
}

case class ArrayLiterNode(es: List[ExprNode])(val pos: (Int, Int))
    extends AssignRHSNode {
    // Note: if es is empty, calling check() leaves typeId as None, and it is
    // the declaration node's responsibility to update it.
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: List[String]): Unit = {
        es.foreach { e => {
                    e.check(st, errors)
                    if (e.typeId == None) {
                        return ()
                    }
        } }
        val typeToCheck = es(0).typeId.get
        val typesMatch = es.forall(e => { e.typeId.get == typeToCheck })
        if (!typesMatch) {
            errors :+ "expression is not of type pair"
            return ()
        }
        typeToCheck match {
            case ArrayType(t, d) => this.typeId = Some(ArrayType(t, d + 1))
            case _               => this.typeId = Some(ArrayType(typeToCheck.getType(), 1))
        }
        
    }
}

object ArrayLiterNode {
    def apply(es: => Parsley[List[ExprNode]]): Parsley[ArrayLiterNode] =
        pos <**> es.map(ArrayLiterNode(_) _)
}
