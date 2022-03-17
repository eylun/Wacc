import parsley.Parsley
import parsley.Parsley._
import parsley.implicits.zipped.{LazyZipped2, LazyZipped3, Zipped4}
import semantics.{typeCheckArithmeticBinOp, typeCheckOrderingBinOp, typeCheckEqualityBinOp, typeCheckLogicalBinOp}
import parsley.errors.combinator.ErrorMethods
import java.util.function.UnaryOperator
import scala.collection.mutable.ListBuffer
import Utility.{lrTypeCheck}

/** ASTNode trait which all other Nodes extend from */
sealed trait ASTNode {

    /** pos: (line number, column number). Stores the position in the input code where the node was generated
      */
    val pos: (Int, Int)

    /** typeId: information about the node's type. Initially set to None as not all nodes require a type (ie. skipNode)
      */
    var typeId: Option[Identifier] = None

    /** Check function checks if the current node is valid. Will generate errors if node is invalid.
      */
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit
}

/** Program Node */
case class ProgramNode(flist: List[FuncNode], s: StatNode)(val pos: (Int, Int)) extends ASTNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {

        /** Check all functions to prevent name clashes. Insert into top level symbol table upon completion, then call
          * each function's check() to check the statements within each function
          */
        flist.foreach {
            case f @ FuncNode(t, i, plist, s) => {
                val funcST = SymbolTable(st)
                st.lookup(i.s) match {
                    case Some(id) =>
                        errors += WaccError(
                          pos,
                          s"${i.s} is already defined in this scope"
                        )

                        /** If a name clash has already happened instantly return
                          */
                        return ()
                    case None => {
                        t.check(st, errors)

                        /** Checks params (uses new symbol table funcST) */
                        plist.reverse.foreach { p =>
                            {
                                p.check(funcST, errors)
                                if (p.typeId.isEmpty) return ()
                            }
                        }

                        /** Set type id as FunctionId */
                        f.typeId = Some(
                          FunctionId(
                            t.typeId.get.getType(),
                            plist
                                .map(p => Param(p.t.typeId.get.getType()))
                                .toArray,
                            funcST
                          )
                        )

                        /** Add function name to outer symbol table. Do this first due to possible recursion
                          */
                        st.add(i.s, f.typeId.get)
                    }

                }
            }
        }
        flist.foreach { f =>
            {
                f.typeId.get match {
                    case FunctionId(_, _, funcST) => f.check(funcST, errors)
                    case _ =>
                        errors += WaccError(
                          pos,
                          "function type allocation malfunction"
                        )
                }
            }
        }

        /** Do not check statement semantics if functions have errors */
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

/** Function Node */
case class FuncNode(
    t: TypeNode,
    i: IdentNode,
    plist: List[ParamNode],
    s: StatNode
)(val pos: (Int, Int))
    extends ASTNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        st.add("return", t.typeId.get.getType())
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

/** Node for function parameters */
case class ParamNode(t: TypeNode, i: IdentNode)(val pos: (Int, Int)) extends ASTNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        st.lookup(i.s) match {
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

/** STATEMENT NODES */
sealed trait StatNode extends ASTNode

case class SkipNode()(val pos: (Int, Int)) extends StatNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object SkipNode extends ParserBuilderPos0[SkipNode]

case class CatchNode(val t: TypeNode, i: IdentNode, s: StatNode)(
    val pos: (Int, Int)
) {
    val catchST = SymbolTable()
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        catchST.setParent(st)
        t.check(catchST, errors)
        catchST.add(i.s, Variable(t.typeId.get.getType()))
        s.check(catchST, errors)
    }
}

object CatchNode {
    def apply(
        t: => Parsley[TypeNode],
        i: => Parsley[IdentNode],
        s: => Parsley[StatNode]
    ): Parsley[CatchNode] =
        pos <**> (t, i, s).lazyZipped(CatchNode(_, _, _) _)
}

case class TryCatchNode(s: StatNode, cs: List[CatchNode])(
    val pos: (Int, Int)
) extends StatNode {
    val tryST = SymbolTable()
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        tryST.setParent(st)
        s.check(tryST, errors)
        cs.foreach(_.check(st, errors))
    }
}

object TryCatchNode {
    def apply(
        s: => Parsley[StatNode],
        cs: => Parsley[List[CatchNode]]
    ): Parsley[TryCatchNode] =
        pos <**> (s, cs).lazyZipped(TryCatchNode(_, _) _)
}

case class ThrowNode(e: ExprNode)(
    val pos: (Int, Int)
) extends StatNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)
    }
}

object ThrowNode {
    def apply(e: => Parsley[ExprNode]): Parsley[ThrowNode] =
        pos <**> e.map(ThrowNode(_) _)
}

case class NewAssignNode(t: TypeNode, i: IdentNode, r: AssignRHSNode)(
    val pos: (Int, Int)
) extends StatNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        t.check(st, errors)
        r.check(st, errors)

        /** Ensure that RHS checked successfully */
        if (r.typeId.isEmpty) return ()
        if (
          !lrTypeCheck(
            t.typeId.get.getType(),
            r.typeId.get.getType()
          )
        ) {
            errors += WaccError(
              pos,
              s"""variable ${i.s} is assigned incompatible type at ${r.repr()} 
                |(Expected: ${t.typeId.get.getType()}, 
                | Actual: ${r.typeId.get.getType()})""".stripMargin
                  .replaceAll("\n", " ")
            )
        }
        st.lookup(i.s) match {
            case None => {
                st.add(i.s, Variable(t.typeId.get.getType()))
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

case class LRAssignNode(l: AssignLHSNode, r: AssignRHSNode)(val pos: (Int, Int)) extends StatNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        l.check(st, errors)
        r.check(st, errors)

        /** Ensure that lhs and rhs checked successfully */
        if (r.typeId.isEmpty || l.typeId.isEmpty) return ()
        if (!lrTypeCheck(l.typeId.get.getType(), r.typeId.get.getType())) {
            errors += WaccError(
              pos,
              s"variable ${l.repr()} is assigned incompatible type at ${r
                  .repr()} (Expected: ${l.typeId.get
                  .getType()}, Actual: ${r.typeId.get.getType()})"
            )
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        l.check(st, errors)

        /** Ensure that lhs has checked sucessfully. Can only read types of Int or char
          */
        if (l.typeId.isEmpty) return ()
        l.typeId.get.getType() match {
            case IntType() | CharType() => {}
            case _ =>
                errors += WaccError(
                  pos,
                  s"""${l.repr()}'s type is incompatible for reading (Expected: 
                    |INT or CHAR, Actual: ${l.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
        }
    }
}

object ReadNode {
    def apply(l: => Parsley[AssignLHSNode]): Parsley[ReadNode] =
        pos <**> l.map(ReadNode(_) _)
}
case class FreeNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully */
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case ArrayType(_, _, _) | PairType(_, _) => {}
            case _ =>
                errors += WaccError(
                  pos,
                  s"""free expression ${e.repr()}'s type is incompatible 
                    |(Expected: ARRAY or PAIR, Actual: ${e.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
        }
    }
}

object FreeNode {
    def apply(e: => Parsley[ExprNode]): Parsley[FreeNode] =
        pos <**> e.map(FreeNode(_) _)
}
case class ReturnNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully */
        if (e.typeId.isEmpty) return ()
        st.lookupAll("return") match {
            case Some(id) => {
                if (!lrTypeCheck(e.typeId.get.getType(), id.getType())) {
                    errors += WaccError(
                      pos,
                      s"""return expression ${e.repr()}'s type is incompatible 
                        |(Expected: ${id
                          .getType()}, Actual: ${e.typeId.get
                          .getType()})""".stripMargin.replaceAll("\n", " ")
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
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully */
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case IntType() => {
                this.typeId = Some(IntType())
            }
            case _ =>
                errors += WaccError(
                  e.pos,
                  s"""exit expression ${e.repr()}'s type is incompatible
                    |(Expected: INT, Actual: ${e.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
        }
    }
}

object ExitNode {
    def apply(e: => Parsley[ExprNode]): Parsley[ExitNode] =
        pos <**> e.map(ExitNode(_) _)
}

case class PrintNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully. If Expr is a function type, generate an error.
          */
        if (e.typeId.isEmpty) return ()
        e.typeId.get match {
            case (FunctionId(_, _, _)) => {
                errors += WaccError(
                  e.pos,
                  s"""print expression ${e.repr()} is a function, which is 
                  | incompatible""".stripMargin.replaceAll("\n", " ")
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

/** println can print any literal or arrays. It CANNOT print functions. Expr can be any literal or identifier, which can
  * be a function.
  */
case class PrintlnNode(e: ExprNode)(val pos: (Int, Int)) extends StatNode {

    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully. If Expr is a function type, generate an error.
          */
        if (e.typeId.isEmpty) return ()
        e.typeId.get match {
            case (FunctionId(_, _, _)) => {
                errors += WaccError(
                  e.pos,
                  s"""println expression ${e.repr()} is a function, which is 
                  |incompatible""".stripMargin.replaceAll("\n", " ")
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
    val trueST = SymbolTable()
    val falseST = SymbolTable()
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully. Expr must be a conditional expr (boolean)
          */
        trueST.setParent(st)
        falseST.setParent(st)
        if (e.typeId.isDefined) {
            e.typeId.get.getType() match {
                case BoolType() => ()
                case _ =>
                    errors += WaccError(
                      e.pos,
                      s"""expression ${e.repr()}'s type is incompatible 
                      | (Expected: BOOL, Actual: ${e.typeId.get.getType()}
                      |)""".stripMargin.replaceAll("\n", " ")
                    )
            }
        }

        s1.check(trueST, errors)
        s2.check(falseST, errors)
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
case class WhileDoNode(e: ExprNode, s: StatNode)(val pos: (Int, Int)) extends StatNode {
    val newScopeST = SymbolTable()
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully. Expr must be a conditional expr (boolean)
          */
        newScopeST.setParent(st)
        if (e.typeId.isDefined) {
            e.typeId.get.getType() match {
                case BoolType() => ()
                case _ =>
                    errors += WaccError(
                      e.pos,
                      s"""expression ${e.repr()}'s type is incompatible 
                      |(Expected: BOOL, Actual: ${e.typeId.get
                          .getType()})""".stripMargin.replaceAll("\n", " ")
                    )
            }
        }
        s.check(newScopeST, errors)
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
    val newScopeST = SymbolTable()
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {

        /** Creates new symbol table and link with outer scope */
        newScopeST.setParent(st)
        s.check(newScopeST, errors)
    }
}

object BeginEndNode {
    def apply(s: => Parsley[StatNode]): Parsley[BeginEndNode] =
        pos <**> s.map(BeginEndNode(_) _)
}

case class StatListNode(s: List[StatNode])(val pos: (Int, Int)) extends StatNode {

    /** Checks every statement */
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        s.foreach { stat => stat.check(st, errors) }
    }
}
object StatListNode {
    def apply(s: => Parsley[List[StatNode]]): Parsley[StatListNode] =
        pos <**> s.map(StatListNode(_) _)
}

/** Assign LHS trait */
sealed trait AssignLHSNode extends ASTNode {
    def repr(): String
}

/** Assign RHS trait */
sealed trait AssignRHSNode extends ASTNode {
    def repr(): String
}

case class MapNode(i: IdentNode, e: ExprNode)(val pos: (Int, Int)) extends AssignRHSNode {

    override def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        i.check(st, errors)
        e.check(st, errors)

        val func = st.lookupAll(i.s)
        func match {
            /** Ident func must only take in 1 argument */
            case Some(FunctionId(_, p, _)) if p.length != 1 => {
                errors += WaccError(i.pos, s"function ${i.repr()} should only have 1 parameter")
                return
            }
            case Some(FunctionId(_, p, _)) if p.length == 1 =>
            /** Ident must be an existing function */
            case _ => {
                errors += WaccError(i.pos, s"identifier ${i.repr()} is not a function")
                return
            }
        }

        val FunctionId(ret, params, funcST) = func.get
        val ptype = params.head.getType()
        e.typeId.get.getType() match {
            /** Ident func's argument must have the same type as expr's element type */
            case ArrayType(t, _, 1) if !lrTypeCheck(ptype, t) => {
                errors += WaccError(
                  e.pos,
                  s"""elements of ${e.repr()} has incompatible type
                      |(Expected: ${ptype}, Actual: $t)""".stripMargin.replaceAll("\n", " ")
                )
                return
            }
            case ArrayType(t, _, 1) if lrTypeCheck(ptype, t) =>
            case ArrayType(t, l, d) if !lrTypeCheck(ptype, ArrayType(t, l, d - 1)) => {
                errors += WaccError(
                  e.pos,
                  s"""elements of ${e.repr()} has incompatible type
					|(Expected: ${ptype}, Actual: ${ArrayType(t, l, d - 1)})""".stripMargin.replaceAll("\n", " ")
                )
                return
            }
            case ArrayType(t, l, d) if lrTypeCheck(ptype, ArrayType(t, l, d - 1)) =>
            /** Expression must be an array type */
            case _ => {
                errors += WaccError(
                  e.pos,
                  s"""expression ${e.repr()}'s type is incompatible 
                      |(Expected: ARRAY, Actual: ${e.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
                return
            }
        }

        val ArrayType(at, al, d) = e.typeId.get.getType()
        typeId = Some(ArrayType(ret, al, d))
    }

    override def repr(): String = s"map ${i.repr()} ${e.repr()}"
}
object MapNode {
    def apply(
        i: => Parsley[IdentNode],
        e: => Parsley[ExprNode]
    ): Parsley[MapNode] =
        pos <**> (i, e).lazyZipped(MapNode(_, _) _)
}
case class FoldNode(i: IdentNode, e1: ExprNode, e2: ExprNode)(
    val pos: (Int, Int)
) extends AssignRHSNode {

    override def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        i.check(st, errors)
        e1.check(st, errors)
        e2.check(st, errors)

        val func = st.lookupAll(i.s)
        func match {
            /** Ident func must only take in 2 arguments */
            case Some(FunctionId(_, p, _)) if p.length != 2 => {
                errors += WaccError(i.pos, s"function ${i.repr()} should only have 1 parameter")
                return
            }
            case Some(FunctionId(_, p, _)) if p.length == 2 =>
            /** Ident must be an existing function */
            case _ => {
                errors += WaccError(i.pos, s"identifier ${i.repr()} is not a function")
                return
            }
        }

        val FunctionId(ret, params, funcST) = func.get

        /** Ident func's 2 arguments must be the same type */
        params match {
            case Array(Param(t1), Param(t2)) if lrTypeCheck(t1, t2) =>
            case _ => {
                errors += WaccError(
                  i.pos,
                  s"function ${i.repr()}'s parameters should be of the same type"
                )
                return
            }
        }

        /** By this point, we know both params are the same type, so just get type of first param (head) */
        val ptype = params.head.getType()

        /** Ident func must return type bool */
        if (!lrTypeCheck(ret, ptype)) {
            errors += WaccError(
              i.pos,
              s"function ${i.repr()}'s return type should the same type as its parameters"
            )
            return
        }

        e2.typeId.get.getType() match {
            /** Ident func's argument must have the same type as expr's element type */
            case ArrayType(t, _, 1) if !lrTypeCheck(ptype, t) => {
                errors += WaccError(
                  e2.pos,
                  s"""elements of ${e2.repr()} has incompatible type
                      |(Expected: ${ptype}, Actual: $t)""".stripMargin.replaceAll("\n", " ")
                )
                return
            }
            case ArrayType(t, _, 1) if lrTypeCheck(ptype, t) =>
            case ArrayType(t, l, d) if lrTypeCheck(ptype, ArrayType(t, l, d - 1)) => {
                errors += WaccError(
                  e2.pos,
                  s"""elements of ${e2.repr()} has incompatible type
					|(Expected: ${ptype}, Actual: ${ArrayType(t, l, d - 1)})""".stripMargin.replaceAll("\n", " ")
                )
                return
            }
            case ArrayType(t, l, d) if !lrTypeCheck(ptype, ArrayType(t, l, d - 1)) =>

            /** Expression 2 must be an array type */
            case _ => {
                errors += WaccError(
                  e2.pos,
                  s"""expression ${e2.repr()}'s type is incompatible 
                      |(Expected: ARRAY, Actual: ${e2.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
                return
            }
        }
        typeId = e1.typeId
    }

    override def repr(): String = s"fold ${i.repr()} ${e1.repr()} ${e2.repr()}"
}

object FoldNode {
    def apply(
        i: => Parsley[IdentNode],
        e1: => Parsley[ExprNode],
        e2: => Parsley[ExprNode]
    ): Parsley[FoldNode] =
        pos <**> (i, e1, e2).lazyZipped(FoldNode(_, _, _) _)
}

case class ScanNode(i: IdentNode, e1: ExprNode, e2: ExprNode)(
    val pos: (Int, Int)
) extends AssignRHSNode {

    override def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        i.check(st, errors)
        e1.check(st, errors)
        e2.check(st, errors)

        val func = st.lookupAll(i.s)
        func match {
            /** Ident func must only take in 2 arguments */
            case Some(FunctionId(_, p, _)) if p.length != 2 => {
                errors += WaccError(i.pos, s"function ${i.repr()} should only have 1 parameter")
                return
            }
            case Some(FunctionId(_, p, _)) if p.length == 2 =>
            /** Ident must be an existing function */
            case _ => {
                errors += WaccError(i.pos, s"identifier ${i.repr()} is not a function")
                return
            }
        }

        val FunctionId(ret, params, funcST) = func.get

        /** Ident func's 2 arguments must be the same type */
        params match {
            case Array(Param(t1), Param(t2)) if lrTypeCheck(t1, t2) =>
            case _ => {
                errors += WaccError(
                  i.pos,
                  s"function ${i.repr()}'s parameters should be of the same type"
                )
                return
            }
        }

        /** By this point, we know both params are the same type, so just get type of first param (head) */
        val ptype = params.head.getType()

        /** Ident func must return type bool */
        if (!lrTypeCheck(ret, ptype)) {
            errors += WaccError(
              i.pos,
              s"function ${i.repr()}'s return type should the same type as its parameters"
            )
            return
        }

        e2.typeId.get.getType() match {
            /** Ident func's argument must have the same type as expr's element type */
            case ArrayType(t, _, 1) if !lrTypeCheck(ptype, t) => {
                errors += WaccError(
                  e2.pos,
                  s"""elements of ${e2.repr()} has incompatible type
                      |(Expected: ${ptype}, Actual: $t)""".stripMargin.replaceAll("\n", " ")
                )
                return
            }
            case ArrayType(t, l, 1) if lrTypeCheck(ptype, t) =>
            case ArrayType(t, l, d) if !lrTypeCheck(ptype, ArrayType(t, l, d - 1)) => {
                errors += WaccError(
                  e2.pos,
                  s"""elements of ${e2.repr()} has incompatible type
					|(Expected: ${ptype}, Actual: ${ArrayType(t, l, d - 1)})""".stripMargin.replaceAll("\n", " ")
                )
                return
            }
            case ArrayType(t, l, d) if lrTypeCheck(ptype, ArrayType(t, l, d - 1)) =>

            /** Expression 2 must be an array type */
            case _ => {
                errors += WaccError(
                  e2.pos,
                  s"""expression ${e2.repr()}'s type is incompatible 
                      |(Expected: ARRAY, Actual: ${e2.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
                return
            }
        }
        typeId = e2.typeId
    }

    override def repr(): String = s"scan ${i.repr()} ${e1.repr()} ${e2.repr()}"
}

object ScanNode {
    def apply(
        i: => Parsley[IdentNode],
        e1: => Parsley[ExprNode],
        e2: => Parsley[ExprNode]
    ): Parsley[ScanNode] =
        pos <**> (i, e1, e2).lazyZipped(ScanNode(_, _, _) _)
}
case class NewPairNode(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends AssignRHSNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e1.check(st, errors)
        e2.check(st, errors)

        /** Ensure that pair elements has checked successfully */
        if (e1.typeId.isEmpty || e2.typeId.isEmpty) return ()
        this.typeId = Some(
          PairType(
            e1.typeId.get.getType(),
            e2.typeId.get.getType()
          )
        )
    }
    def repr(): String = s"newpair (${e1.repr()}, ${e2.repr()})"
}

object NewPairNode {
    def apply(
        e1: => Parsley[ExprNode],
        e2: => Parsley[ExprNode]
    ): Parsley[NewPairNode] =
        pos <**> (e1, e2).lazyZipped(NewPairNode(_, _) _)
}

case class CallNode(i: IdentNode, args: List[ExprNode])(val pos: (Int, Int)) extends AssignRHSNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {

        /** Set type to be equal to function return type if it exists in symbol table
          */
        st.lookupAll(i.s) match {
            case Some(FunctionId(returnType, params, _)) => {
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
                      s"""Incorrect number of parameters for ${i
                          .repr()} (Expected: ${params.length}, 
                        | Actual: ${args.length})""".stripMargin
                          .replaceAll("\n", " ")
                    )
                    return ()
                }

                /** Compare arg types against param list */
                for ((arg, index) <- args.zipWithIndex) {
                    if (index < paramTypes.length) {
                        if (
                          !lrTypeCheck(
                            arg.typeId.get.getType(),
                            params(index).getType()
                          )
                        ) {
                            errors += WaccError(
                              arg.pos,
                              s"""argument ${arg.repr()}'s type is incompatible 
                              | (Expected: ${params(
                                index
                              )}, Actual: ${arg.typeId.get
                                  .getType()})""".stripMargin
                                  .replaceAll("\n", " ")
                            )
                            return ()
                        }
                    } else {
                        errors += WaccError(
                          pos,
                          s"""Incorrect number of parameters for ${i.repr()} 
                              |(Expected: ${params.length}, 
                              ||Actual: ${args.length})""".stripMargin
                              .replaceAll("\n", " ")
                        )
                    }
                }
            }
            case _ =>
                errors += WaccError(
                  pos,
                  s"${i.repr()} is not defined as a function"
                )
        }

    }
    def repr() =
        s"call ${i.repr()}(${args.map(a => a.repr()).mkString(", ")})"
}

object CallNode {
    def apply(
        i: => Parsley[IdentNode],
        args: => Parsley[List[ExprNode]]
    ): Parsley[CallNode] =
        pos <**> (i, args).lazyZipped(CallNode(_, _) _)
}

/** TYPE NODES */
/** Type Node trait which all other Type Nodes extend from */
sealed trait TypeNode extends ASTNode

/** BASE TYPE NODES */
sealed trait BaseTypeNode extends TypeNode with PairElemTypeNode

case class ExceptionTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    typeId = Some(ExceptionType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object ExceptionTypeNode extends ParserBuilderPos0[ExceptionTypeNode]

case class IntTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    typeId = Some(IntType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object IntTypeNode extends ParserBuilderPos0[IntTypeNode]

case class BoolTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    typeId = Some(BoolType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object BoolTypeNode extends ParserBuilderPos0[BoolTypeNode]

case class CharTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    typeId = Some(CharType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}
object CharTypeNode extends ParserBuilderPos0[CharTypeNode]

case class StringTypeNode()(val pos: (Int, Int)) extends BaseTypeNode {
    typeId = Some(StringType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object StringTypeNode extends ParserBuilderPos0[StringTypeNode]

/** Pair Type Node */
case class PairTypeNode(fst: PairElemTypeNode, snd: PairElemTypeNode)(
    val pos: (Int, Int)
) extends TypeNode
    with PairElemTypeNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        fst.check(st, errors)
        snd.check(st, errors)

        /** Ensure that pair elements has checked successfully */
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

/** Pair Element Type Node */
sealed trait PairElemTypeNode extends TypeNode

/** For the case where just 'pair' is parsed */
case class PairElemTypePairNode()(val pos: (Int, Int)) extends PairElemTypeNode {
    typeId = Some(NestedPairType())
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object PairElemTypePairNode extends ParserBuilderPos0[PairElemTypePairNode]

/** EXPRESSION NODES */
sealed trait ExprNode extends AssignRHSNode

/** ParserBuilder for operator builder patterns implementors of ParserBuilder can serve as a builder of parsers of type
  * T
  */
trait ParserBuilder[T] {
    val parser: Parsley[T]
    final def <#(p: Parsley[_]): Parsley[T] =
        parser <* p.label(
          "operators: { *, /, %, +, -, >, >=, <, <=, ==, !=, &&, ||}"
        )
}

trait ParserBuilderPos0[R] extends ParserBuilder[R] {
    def apply()(pos: (Int, Int)): R
    val parser = pos.map(p => apply()(p))
}

trait ParserBuilderPos1[T1, R] extends ParserBuilder[T1 => R] {
    def apply(x: T1)(pos: (Int, Int)): R
    val parser = pos.map(p => (apply(_)(p)))
}

/** UNARY OPERATOR NODES */
sealed trait UnaryOpNode extends ExprNode

/** Array Type */
/* Special Representation: dimension tracks how many dimensions the identifier's
 * array is. This is inserted upon parsing to make it less tedious for
 * semantic checking.
 */
case class ArrayTypeNode(t: TypeNode, dimension: Int)(val pos: (Int, Int))
    extends PairElemTypeNode
    with UnaryOpNode
    with TypeNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        t.check(st, errors)
        this.typeId = Some(
          ArrayType(t.typeId.get.getType(), dimension)
        )
    }
    def repr(): String = s"${t.typeId.get.getType()} ${"[]" * dimension}"
}
object ArrayTypeNode {

    /** Apply function for chain operation */
    def apply(op: => Parsley[Unit]): Parsley[ArrayTypeNode => ArrayTypeNode] =
        pos.map[ArrayTypeNode => ArrayTypeNode](p =>
            ((at: ArrayTypeNode) => at.copy(dimension = at.dimension + 1)(p))
        ) <* op

    /** Apply function for first creation */
    def apply(
        t: => Parsley[TypeNode],
        dimension: => Int
    ): Parsley[ArrayTypeNode] =
        pos <**> t.map(ArrayTypeNode(_, dimension) _)
}

/** Not: Logical Not */
case class Not(e: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully. Must be boolean */
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case BoolType() =>
                this.typeId = Some(BoolType())
            case _ =>
                errors += WaccError(
                  e.pos,
                  s"""expression ${e.repr()}'s type is incompatible for the '!'
                  |operator (Expected: BOOL, Actual: ${e.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
        }

    }
    def repr(): String = s"(!${e.repr()})"
}

/** Ops(Prefix)(Not <# 'not') ----> returns Parser of type Not */
object Not extends ParserBuilderPos1[ExprNode, Not]

/** Neg: Negation for integers */
case class Neg(e: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully. Must be an integer
          */
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case IntType() =>
                this.typeId = Some(IntType())
            case _ =>
                errors += WaccError(
                  e.pos,
                  s"""expression ${e.repr()}'s type is incompatible for the '-'
                  |operator (Expected: INT, Actual: ${e.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
        }

    }
    def repr(): String = s"(- ${e.repr()})"
}

object Neg extends ParserBuilderPos1[ExprNode, Neg]

/** Len: Array length operator */
case class Len(e: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully. Must be an array of any type
          */
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case ArrayType(_, _, _) =>
                this.typeId = Some(IntType())
            case _ =>
                errors += WaccError(
                  e.pos,
                  s"""expression ${e.repr()}'s type is incompatible for the
                  |'len' operator (Expected: ANY[], Actual: ${e.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
        }

    }
    def repr(): String = s"(len ${e.repr()})"
}

object Len extends ParserBuilderPos1[ExprNode, Len]

/** Ord: Returns Unicode Index of a character */
case class Ord(e: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {

    /** add list of wacc errors as parameter later */
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully. Expects expression of type character
          */
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case CharType() =>
                this.typeId = Some(IntType())
            case _ =>
                errors += WaccError(
                  e.pos,
                  s"""expression ${e.repr()}'s type is incompatible for the
                  |'ord' operator (Expected: CHAR, Actual: ${e.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
        }

    }
    def repr(): String = s"(ord ${e.repr()})"
}

object Ord extends ParserBuilderPos1[ExprNode, Ord]

/** Chr: Returns Unicode Representation of an integer */
case class Chr(e: ExprNode)(val pos: (Int, Int)) extends UnaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully. Must be type int */
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case IntType() =>
                this.typeId = Some(CharType())
            case _ =>
                errors += WaccError(
                  e.pos,
                  s"""expression ${e.repr()}'s type is incompatible for the
                  |'chr' operator (Expected: INT, Actual: ${e.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
        }

    }
    def repr(): String = s"(chr ${e.repr()})"
}

object Chr extends ParserBuilderPos1[ExprNode, Chr]

trait ParserBuilderPos2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
    def apply(x: T1, y: T2)(pos: (Int, Int)): R
    val parser = pos.map(p => apply(_, _)(p))
}

/** BINARY OPERATOR NODES */
sealed trait BinaryOpNode extends ExprNode {
    def symbol(): String
}

/** Integer Multiplication */
case class Mult(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckArithmeticBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} * ${e2.repr()})"
    def symbol(): String = "*"
}

object Mult extends ParserBuilderPos2[ExprNode, ExprNode, Mult]

/** Integer Division */
case class Div(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckArithmeticBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} / ${e2.repr()})"
    def symbol(): String = "/"
}

object Div extends ParserBuilderPos2[ExprNode, ExprNode, Div]

/** Modulus */
case class Mod(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckArithmeticBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} % ${e2.repr()})"
    def symbol(): String = "%"
}

object Mod extends ParserBuilderPos2[ExprNode, ExprNode, Mod]

/** Integer Addition */
case class Add(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckArithmeticBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} + ${e2.repr()})"
    def symbol(): String = "+"
}

object Add extends ParserBuilderPos2[ExprNode, ExprNode, Add]

/** Integer Subtraction */
case class Sub(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckArithmeticBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} - ${e2.repr()})"
    def symbol(): String = "-"
}

object Sub extends ParserBuilderPos2[ExprNode, ExprNode, Sub]

/** Greater Than Operator */
case class GT(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckOrderingBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} > ${e2.repr()})"
    def symbol(): String = ">"
}

object GT extends ParserBuilderPos2[ExprNode, ExprNode, GT]

/** Greater Than or Equal Operator */
case class GTE(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckOrderingBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} >= ${e2.repr()})"
    def symbol(): String = ">="
}

object GTE extends ParserBuilderPos2[ExprNode, ExprNode, GTE]

/** Less Than Operator */
case class LT(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckOrderingBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} < ${e2.repr()})"
    def symbol(): String = "<"
}

object LT extends ParserBuilderPos2[ExprNode, ExprNode, LT]

/** Less Than or Equal Operator */
case class LTE(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckOrderingBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} <= ${e2.repr()})"
    def symbol(): String = "<="
}

object LTE extends ParserBuilderPos2[ExprNode, ExprNode, LTE]

/** Equality Operator */
case class Equal(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckEqualityBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} == ${e2.repr()})"
    def symbol(): String = "=="
}

object Equal extends ParserBuilderPos2[ExprNode, ExprNode, Equal]

/** Inequality Operator */
case class NotEqual(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckEqualityBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} != ${e2.repr()})"
    def symbol(): String = "!="
}

object NotEqual extends ParserBuilderPos2[ExprNode, ExprNode, NotEqual]

/** Logical And */
case class And(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckLogicalBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} && ${e2.repr()})"
    def symbol(): String = "&&"
}

object And extends ParserBuilderPos2[ExprNode, ExprNode, And]

/** Logical Or */
case class Or(e1: ExprNode, e2: ExprNode)(val pos: (Int, Int)) extends BinaryOpNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        typeCheckLogicalBinOp(pos, st, this, e1, e2, errors)
    }
    def repr(): String = s"(${e1.repr()} || ${e2.repr()})"
    def symbol(): String = "||"
}

object Or extends ParserBuilderPos2[ExprNode, ExprNode, Or]

/** Identifier Node */
case class IdentNode(var s: String)(val pos: (Int, Int)) extends ExprNode with AssignLHSNode {
    def repr(): String = s
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {

        st.lookupAll(s) match {
            case None =>
                errors += WaccError(
                  pos,
                  s"$s has not been defined in this scope"
                )
            case i @ Some(_) => this.typeId = i
        }
    }
}

object IdentNode {
    def apply(s: => Parsley[String]): Parsley[IdentNode] =
        pos <**> s.map(IdentNode(_) _)
}

/** Array Elements */
case class ArrayElemNode(i: IdentNode, es: List[ExprNode])(val pos: (Int, Int)) extends ExprNode with AssignLHSNode {

    def repr(): String = {
        s"${i.repr()}[${es.map { e => e.repr() }.mkString("\n")}]"
    }
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        i.check(st, errors)
        es.foreach { e =>
            {
                e.check(st, errors)

                /** Ensure that expression has checked successfully */
                if (e.typeId.isEmpty) return ()
                e.typeId.get.getType() match {
                    case IntType() => ()
                    case _ => {
                        errors += WaccError(
                          e.pos,
                          s"""expression ${e.repr()}'s type is incompatible for 
                              |array element evaluation (Expected: INT, 
                              |Actual: ${e.typeId.get
                              .getType()})""".stripMargin.replaceAll("\n", " ")
                        )
                        return ()
                    }
                }
            }
        }

        /** Ensure that identifier has checked successfully */
        if (i.typeId.isEmpty) return ()
        i.typeId.get.getType() match {
            case ArrayType(t, l, d) => {
                d.compare(es.length) match {
                    case -1 =>
                        errors += WaccError(
                          i.pos,
                          s"""array element type incompatible (Expected: 
                              |Any${"[]" * es.length}, Actual: ${t
                              .getType()}${"[]" * d})""".stripMargin
                              .replaceAll("\n", " ")
                        )
                    case 0 => this.typeId = Some(t)
                    case 1 => this.typeId = Some(ArrayType(t, l, d - es.length))
                }
            }
            case t => {
                errors += WaccError(
                  i.pos,
                  s"""array element type incompatible (Expected: 
                    |Any${"[]" * es.length}, Actual: ${t
                      .getType()})""".stripMargin.replaceAll("\n", " ")
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

/** PAIR ELEMENT NODES */
sealed trait PairElemNode extends AssignLHSNode with AssignRHSNode

case class FirstPairElemNode(e: ExprNode)(val pos: (Int, Int)) extends PairElemNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully */
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case PairType(t, _) => this.typeId = Some(t)
            case NullPairType() =>
                errors += WaccError(
                  e.pos,
                  s"accessing element of null pair"
                )
            case _ =>
                errors += WaccError(
                  e.pos,
                  s"""expression ${e.repr()} type incompatible for 'fst' 
                  |(Expected: PAIR(ANY, ANY), Actual: ${e.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
        }

    }
    def repr(): String = s"fst ${e.repr()}"
}

object FirstPairElemNode {
    def apply(e: => Parsley[ExprNode]): Parsley[FirstPairElemNode] =
        pos <**> e.map(FirstPairElemNode(_) _)
}

case class SecondPairElemNode(e: ExprNode)(val pos: (Int, Int)) extends PairElemNode {
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        e.check(st, errors)

        /** Ensure that expression has checked successfully */
        if (e.typeId.isEmpty) return ()
        e.typeId.get.getType() match {
            case PairType(_, t) => this.typeId = Some(t)
            case NullPairType() =>
                errors += WaccError(
                  e.pos,
                  s"accessing element of null pair"
                )
            case _ =>
                errors += WaccError(
                  e.pos,
                  s"""expression ${e.repr()} type incompatible for 'snd' 
                  |(Expected: PAIR(ANY, ANY) , Actual: ${e.typeId.get
                      .getType()})""".stripMargin.replaceAll("\n", " ")
                )
        }

    }
    def repr(): String = s"snd ${e.repr()}"
}

object SecondPairElemNode {
    def apply(e: => Parsley[ExprNode]): Parsley[SecondPairElemNode] =
        pos <**> e.map(SecondPairElemNode(_) _)
}

/** LITERALS */
case class IntLiterNode(i: Int)(val pos: (Int, Int)) extends ExprNode {
    typeId = Some(IntType())

    def repr(): String = {
        i.toString()
    }
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object IntLiterNode {
    def apply(i: => Parsley[Int]): Parsley[IntLiterNode] =
        pos <**> i.map(IntLiterNode(_) _)
}

case class BoolLiterNode(b: Boolean)(val pos: (Int, Int)) extends ExprNode {
    typeId = Some(BoolType())

    def repr(): String = {
        b.toString()
    }
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object BoolLiterNode {
    def apply(b: => Parsley[Boolean]): Parsley[BoolLiterNode] =
        pos <**> b.map(BoolLiterNode(_) _)
}

case class CharLiterNode(c: Char)(val pos: (Int, Int)) extends ExprNode {
    typeId = Some(CharType())

    def repr(): String = s"'$c'"
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object CharLiterNode {
    def apply(c: => Parsley[Char]): Parsley[CharLiterNode] =
        pos <**> c.map(CharLiterNode(_) _)
}

case class StringLiterNode(s: String)(val pos: (Int, Int)) extends ExprNode {
    typeId = Some(StringType())
    def repr(): String = s"\"$s\""
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object StringLiterNode {
    def apply(s: => Parsley[String]): Parsley[StringLiterNode] =
        pos <**> s.map(StringLiterNode(_) _)
}

case class PairLiterNode()(val pos: (Int, Int)) extends ExprNode {
    typeId = Some(NullPairType())

    def repr(): String = "null"
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {}
}

object PairLiterNode {
    def apply(): Parsley[PairLiterNode] = pos.map(p => new PairLiterNode()(p))
}

case class ArrayLiterNode(es: List[ExprNode])(val pos: (Int, Int)) extends AssignRHSNode {

    /** Note: if es is empty, calling check() leaves typeId as None, and it is the declaration node's responsibility to
      * update it.
      */
    def repr(): String = {
        s"[${es.map { e => e.repr() }.mkString(", ")}]"
    }
    def check(st: SymbolTable, errors: ListBuffer[WaccError]): Unit = {
        if (!es.isEmpty) {
            es.foreach { e =>
                {
                    e.check(st, errors)
                    if (e.typeId.isEmpty) {
                        return ()
                    }
                }
            }
            val typeToCheck = es(0).typeId.get.getType()
            val typesMatch = es.forall(e => {
                e.typeId.get.getType() == typeToCheck
            })
            if (!typesMatch) {
                errors += WaccError(
                  pos,
                  s"array element types are inconsistent"
                )
                return ()
            }
            typeToCheck match {
                case ArrayType(t, l, d) =>
                    typeId = Some(ArrayType(t, l ++ List(es.length), d + 1))
                case _ =>
                    typeId = Some(ArrayType(typeToCheck, List(es.length), 1))
            }
        } else {
            typeId = Some(AnyType())
        }

    }
}

object ArrayLiterNode {
    def apply(es: => Parsley[List[ExprNode]]): Parsley[ArrayLiterNode] =
        pos <**> es.map(ArrayLiterNode(_) _)
}
