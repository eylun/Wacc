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
import scala.collection.mutable.ListBuffer

sealed trait ASTNode {
    var typeId: Option[Identifier]
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit
}
// Program
case class ProgramNode(flist: List[FuncNode], s: StatNode)(val pos: (Int, Int))
    extends ASTNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        flist.foreach { f => f.check(st, errors) }
        // Do not check statement semantics if functions have errors
        if (errors.length > 0) return ()
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        // Check function name does not already exist
        st.lookup(i.s) match {
            case Some(id) =>
                errors += WaccError(
                  pos,
                  s"${i.s} is already defined in this scope"
                )
            case None => {
                t.check(st, errors)

                // Create new symbol table and link with outer scope
                val funcST = SymbolTable(st)

                // Check list of params (using new symbol table funcST)
                plist.foreach { p =>
                    {
                        p.check(funcST, errors)
                        if (p.typeId.isEmpty) return ()
                    }
                }
                // Set type id as FunctionId
                this.typeId = Some(
                  FunctionId(
                    t.typeId.get.getType(),
                    plist.map(p => Param(p.t.typeId.get.getType())).toArray,
                    funcST
                  )
                )

                // Add function name to outer symbol table. DO this first
                // due to possible recursion
                st.add(i.s, this.typeId.get)

                funcST.add("return", t.typeId.get.getType())
            }
        }
        s.check(st, errors)
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        st.lookup(i.s) match {
            case Some(FunctionId(_, _, _)) => {
                st.lookupAll(i.s + "$") match {
                    case None => {
                        st.add(i.s + "$", Param(t.typeId.get.getType()))
                    }
                    case Some(_) =>
                        // Check that parameter has not already been created
                        errors += WaccError(
                          pos,
                          s"parameter ${i.s} is already defined in this scope"
                        )
                }
            }
            case Some(_) =>
                errors += WaccError(
                  pos,
                  s"parameter ${i.s} is already defined in this scope"
                )
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object SkipNode extends ParserBuilderPos0[SkipNode]

case class NewAssignNode(t: TypeNode, i: IdentNode, r: AssignRHSNode)(
    val pos: (Int, Int)
) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        t.check(st, errors)
        r.check(st, errors)
        // Ensure that rhs checked successfully
        if (r.typeId.isEmpty) return ()
        if (r.typeId.get.getType() != t.typeId.get.getType()) {
            r.typeId.get.getType() match {
                case AnyType() => ()
                case _ => {
                    errors += WaccError(
                      pos,
                      s"variable ${i.s} is assigned incompatible type (Expected: ${t.typeId.get}, Actual: ${r.typeId.get})"
                    )
                }
            }
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
                        errors += WaccError(
                          pos,
                          s"variable ${i.s} is assigned within the scope"
                        )
                }
            }
            case _ =>
                errors += WaccError(
                  pos,
                  s"variable ${i.s} is assigned within the scope"
                )
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        l.check(st, errors)
        r.check(st, errors)
        // Ensure that lhs and rhs checked successfully
        if (r.typeId.isEmpty || l.typeId.isEmpty) return ()
        if (l.typeId.get.getType() != r.typeId.get.getType()) {
            r.typeId.get.getType() match {
                // AnyType is triggered when an empty array literal is returned
                case AnyType() => ()
                case _ => {
                    errors += WaccError(
                      pos,
                      s"${l} is assigned incompatible type (Expected: ${l.typeId}, Actual: ${r.typeId})"
                    )
                    return ()
                }
            }
        }
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        l.check(st, errors)
        // Ensure that lhs has checked sucessfully
        if (l.typeId.isEmpty) return ()
        l.typeId.get match {
            case IntType() | CharType() => {}
            case _ =>
                errors += WaccError(
                  pos,
                  s"$l's type is incompatible for reading (Expected: INT or CHAR, Actual: ${l.typeId.get})"
                )
        }
    }
}

object ReadNode {
    def apply(l: => Parsley[AssignLHSNode]): Parsley[ReadNode] =
        pos <**> l.map(ReadNode(_) _)
}
case class FreeNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get match {
            case ArrayType(_, _) | PairType(_, _) => {}
            case _ =>
                errors += WaccError(
                  pos,
                  s"free expression $e's type is incompatible (Expected: INT or CHAR, Actual: ${e.typeId.get})"
                )
        }
    }
}

object FreeNode {
    def apply(e: => Parsley[ExprNode]): Parsley[FreeNode] =
        pos <**> e.map(FreeNode(_) _)
}
case class ReturnNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        st.lookupAll("return") match {
            case Some(id) => {
                if (e.typeId.get.getType() != id.getType()) {
                    errors += WaccError(
                      pos,
                      s"return expression $e's type is incompatible (Expected: $id, Actual: ${e.typeId.get}"
                    )
                }
            }
            case None => {
                errors += WaccError(
                  pos,
                  "return statement called outside of a function scope"
                )
            }
        }
    }
}

object ReturnNode {
    def apply(e: => Parsley[ExprNode]): Parsley[ReturnNode] =
        pos <**> e.map(ReturnNode(_) _)
}

case class ExitNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get match {
            case IntType() | Variable(IntType()) => {
                this.typeId = Some(IntType())
            }
            case _ =>
                errors += WaccError(
                  pos,
                  s"exit expression $e's type is incompatible (Expected: INT, Actual: ${e.typeId.get}"
                )
        }
    }
}

object ExitNode {
    def apply(e: => Parsley[ExprNode]): Parsley[ExitNode] =
        pos <**> e.map(ExitNode(_) _)
}

case class PrintNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get match {
            case (FunctionId(_, _, _)) => {
                errors += WaccError(
                  pos,
                  s"print expression $e is a function, which is incompatible"
                )
            }
            case _ => {}
        }
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get match {
            case (FunctionId(_, _, _)) => {
                errors += WaccError(
                  pos,
                  s"println expression $e is a function, which is incompatible"
                )
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case BoolType() => {
                s1.check(st, errors)
                s2.check(st, errors)
            }
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression $e's type is incompatible (Expected: BOOL, Actual: ${e.typeId.get}"
                )
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get match {
            case BoolType() => {
                s.check(st, errors)
            }
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression $e's type is incompatible (Expected: BOOL, Actual: ${e.typeId.get}"
                )
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e1.check(st, errors)
        e2.check(st, errors)
        // Ensure that expression has checked successfully
        if (e1.typeId.isEmpty || e2.typeId.isEmpty) return ()
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        // set type to be equal to function return type if it exists in symbol table
        st.lookupAll(i.s) match {
            case Some(FunctionId(returnType, params, funcST)) => {
                this.typeId = Some(returnType)
                args.foreach { arg =>
                    {
                        arg.check(st, errors)
                        if (arg.typeId.isEmpty) return ()
                    }
                }
                val paramTypes = params.map { _.getType() }

                if (args.length < paramTypes.length) {
                    errors += WaccError(
                      pos,
                      s"Incorrect number of parameters for $i (Expected: ${params.length}, Actual: ${args.length})"
                    )
                    return ()
                }

                // Compare arg types against param list
                for ((arg, index) <- args.zipWithIndex) {
                    if (index < paramTypes.length) {
                        if (
                          arg.typeId.get.getType() != params(index).getType()
                        ) {
                            errors += WaccError(
                              pos,
                              s"argument $arg's type is incompatible (Expected: ${params(index)}, Actual: ${arg.typeId.get}"
                            )
                            return ()
                        }
                    } else {
                        errors += WaccError(
                          pos,
                          s"Incorrect number of parameters for $i (Expected: ${params.length}, Actual: ${args.length})"
                        )
                    }
                }
            }
            case _ =>
                errors += WaccError(
                  pos,
                  s"$i is not defined as a function"
                )
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object IntTypeNode extends ParserBuilderPos0[IntTypeNode]

case class BoolTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(BoolType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object BoolTypeNode extends ParserBuilderPos0[BoolTypeNode]

case class CharTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(CharType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}
object CharTypeNode extends ParserBuilderPos0[CharTypeNode]

case class StringTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    var typeId: Option[Identifier] = Some(StringType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object StringTypeNode extends ParserBuilderPos0[StringTypeNode]

// Pair Type
case class PairTypeNode(fst: PairElemTypeNode, snd: PairElemTypeNode)(
    val pos: (Int, Int)
) extends TypeNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        fst.check(st, errors)
        snd.check(st, errors)
        // Ensure that pair elements has checked successfully
        if (fst.typeId.isEmpty || snd.typeId.isEmpty) return ()
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object PairElemTypePairNode extends ParserBuilderPos0[PairElemTypePairNode]

// Expression
sealed trait ExprNode extends AssignRHSNode

// ParserBuilder for operator builder patterns
// implementors of ParserBuilder can serve as a builder of parsers of type T
trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] =
        parser <* p.label("operators")
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
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

case class Not(e: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case BoolType() =>
                this.typeId = Some(BoolType())
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression $e's type is incompatible for the '!' operator (Expected: BOOL, Actual: ${e.typeId.get})"
                )
        }
    }
}

// Ops(Prefix)(Not <# 'not') ----> returns Parser of type Not
object Not extends ParserBuilderPos1[ExprNode, Not]

case class Neg(e: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case IntType() =>
                this.typeId = Some(IntType())
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression $e's type is incompatible for the '-' operator (Expected: INT, Actual: ${e.typeId.get})"
                )
        }
    }
}

object Neg extends ParserBuilderPos1[ExprNode, Neg]

case class Len(e: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case ArrayType(_, _) =>
                this.typeId = Some(IntType())
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression $e's type is incompatible for the 'len' operator (Expected: ANY[], Actual: ${e.typeId.get})"
                )
        }
    }
}

object Len extends ParserBuilderPos1[ExprNode, Len]

case class Ord(e: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    // add list of wacc errors as parameter later
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case CharType() =>
                this.typeId = Some(IntType())
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression $e's type is incompatible for the 'ord' operator (Expected: CHAR, Actual: ${e.typeId.get})"
                )
        }
    }
}

object Ord extends ParserBuilderPos1[ExprNode, Ord]

case class Chr(e: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case IntType() =>
                this.typeId = Some(CharType())
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression $e's type is incompatible for the 'chr' operator (Expected: INT, Actual ${e.typeId.get})"
                )
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
case class Mult(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckArithmeticBinOp(pos, st, this, e1, e1, errors)
    }
}

object Mult extends ParserBuilderPos2[ExprNode, ExprNode, Mult]

case class Div(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckArithmeticBinOp(pos, st, this, e1, e2, errors)
    }
}

object Div extends ParserBuilderPos2[ExprNode, ExprNode, Div]
case class Mod(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckArithmeticBinOp(pos, st, this, e1, e2, errors)
    }
}

object Mod extends ParserBuilderPos2[ExprNode, ExprNode, Mod]
case class Add(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckArithmeticBinOp(pos, st, this, e1, e2, errors)
    }
}

object Add extends ParserBuilderPos2[ExprNode, ExprNode, Add]

case class Sub(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckArithmeticBinOp(pos, st, this, e1, e2, errors)
    }
}

object Sub extends ParserBuilderPos2[ExprNode, ExprNode, Sub]

case class GT(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckOrderingBinOp(pos, st, this, e1, e2, errors)
    }
}

object GT extends ParserBuilderPos2[ExprNode, ExprNode, GT]

case class GTE(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckOrderingBinOp(pos, st, this, e1, e2, errors)
    }
}

object GTE extends ParserBuilderPos2[ExprNode, ExprNode, GTE]

case class LT(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckOrderingBinOp(pos, st, this, e1, e2, errors)
    }
}

object LT extends ParserBuilderPos2[ExprNode, ExprNode, LT]

case class LTE(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckOrderingBinOp(pos, st, this, e1, e2, errors)
    }
}

object LTE extends ParserBuilderPos2[ExprNode, ExprNode, LTE]

case class Equal(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckEqualityBinOp(pos, st, this, e1, e2, errors)
    }
}

object Equal extends ParserBuilderPos2[ExprNode, ExprNode, Equal]

case class NotEqual(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckEqualityBinOp(pos, st, this, e1, e2, errors)
    }
}

object NotEqual extends ParserBuilderPos2[ExprNode, ExprNode, NotEqual]

case class And(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckLogicalBinOp(pos, st, this, e1, e2, errors)
    }
}

object And extends ParserBuilderPos2[ExprNode, ExprNode, And]

case class Or(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int))
    extends BinaryOpNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckLogicalBinOp(pos, st, this, e1, e2, errors)
    }
}

object Or extends ParserBuilderPos2[ExprNode, ExprNode, Or]

// Identifier
case class IdentNode(var s: String)(val pos: (Int, Int))
    extends ExprNode
    with AssignLHSNode {
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {

        st.lookupAll(s) match {
            case None =>
                errors += WaccError(
                  pos,
                  s"$s has not been defined in this scope"
                )
            case Some(FunctionId(_, _, _)) => {
                st.lookupAll(s + "$") match {
                    case None =>
                        errors += WaccError(
                          pos,
                          s"$s has not been defined in this scope"
                        )
                    case i @ Some(_) => this.typeId = i
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        i.check(st, errors)
        es.foreach { e =>
            {
                e.check(st, errors)
                e.typeId match {
                    case None => return ()
                    case Some(IntType()) | Some(Variable(IntType())) => ()
                    case _ => {
                        errors += WaccError(
                          pos,
                          s"expression $e's type is incompatible for array element evaluation (Expected: INT, Actual: ${e.typeId.get}"
                        )
                        return ()
                    }
                }
            }
        }

        i.typeId match {
            case None => return ()
            case Some(ArrayType(t, d)) => {

                d.compare(es.length) match {
                    case -1 =>
                        errors += WaccError(
                          pos,
                          s"array element type imcompatible (Expected: Any${"[]" * es.length}, Actual: ${t
                              .getType()}${"[]" * d})"
                        )
                    case 0 => this.typeId = Some(t)
                    case 1 => this.typeId = Some(ArrayType(t, d - es.length))
                }
            }
            case Some(t) => {
                errors += WaccError(
                  pos,
                  s"array element type imcompatible (Expected: Any${"[]" * es.length}, Actual: ${t
                      .getType()})"
                )
            }

        }

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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case PairType(t, _) => this.typeId = Some(t)
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression $e type imcompatible for 'fst' (Expected: PAIR, Actual: ${e.typeId.get})"
                )
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
        // Ensure that expression has checked successfully
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case PairType(_, t) => this.typeId = Some(t)
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression ${e} type imconpatible for 'snd' (Expected: , Actual: ${e.typeId.get})"
                )
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object IntLiterNode {
    def apply(i: => Parsley[Int]): Parsley[IntLiterNode] =
        pos <**> i.map(IntLiterNode(_) _)
}

case class BoolLiterNode(b: Boolean)(val pos: (Int, Int)) extends ExprNode {
    var typeId: Option[Identifier] = Some(BoolType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object BoolLiterNode {
    def apply(b: => Parsley[Boolean]): Parsley[BoolLiterNode] =
        pos <**> b.map(BoolLiterNode(_) _)
}

case class CharLiterNode(c: Char)(val pos: (Int, Int)) extends ExprNode {
    var typeId: Option[Identifier] = Some(CharType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object CharLiterNode {
    def apply(c: => Parsley[Char]): Parsley[CharLiterNode] =
        pos <**> c.map(CharLiterNode(_) _)
}

case class StringLiterNode(s: String)(val pos: (Int, Int)) extends ExprNode {
    var typeId: Option[Identifier] = Some(StringType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object StringLiterNode {
    def apply(s: => Parsley[String]): Parsley[StringLiterNode] =
        pos <**> s.map(StringLiterNode(_) _)
}

class PairLiterNode()(val pos: (Int, Int)) extends ExprNode {
    var typeId: Option[Identifier] = Some(NullPairType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object PairLiterNode {
    def apply(): Parsley[PairLiterNode] = pos.map(p => new PairLiterNode()(p))
}

case class ArrayLiterNode(es: List[ExprNode])(val pos: (Int, Int))
    extends AssignRHSNode {
    // Note: if es is empty, calling check() leaves typeId as None, and it is
    // the declaration node's responsibility to update it.
    var typeId: Option[Identifier] = None
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        if (!es.isEmpty) {
            es.foreach { e =>
                {
                    e.check(st, errors)
                    if (e.typeId == None) {
                        return ()
                    }
                }
            }
            val typeToCheck = es(0).typeId.get
            val typesMatch = es.forall(e => { e.typeId.get == typeToCheck })
            if (!typesMatch) {
                errors += WaccError(
                  pos,
                  s"array element types are inconsistent"
                )
                return ()
            }
            typeToCheck match {
                case ArrayType(t, d) => this.typeId = Some(ArrayType(t, d + 1))
                case _ =>
                    this.typeId = Some(ArrayType(typeToCheck.getType(), 1))
            }
        } else {
            this.typeId = Some(AnyType())
        }

    }
}

object ArrayLiterNode {
    def apply(es: => Parsley[List[ExprNode]]): Parsley[ArrayLiterNode] =
        pos <**> es.map(ArrayLiterNode(_) _)
}
