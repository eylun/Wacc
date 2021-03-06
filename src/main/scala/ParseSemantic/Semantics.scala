import scala.collection.mutable.ListBuffer
import Utility.{lrTypeCheck}

/** Abstracted semantic checking for binary operations */
object semantics {

    /** Arithmetic binary operators
      *
      * Check types of arithmetic binary operations (Mult, Div, Mod, Add, Sub).
      * Arguments can be literals, variables, array elements.
      */
    def typeCheckArithmeticBinOp(
        pos: (Int, Int),
        st: SymbolTable,
        op: BinaryOpNode,
        x: ExprNode,
        y: ExprNode,
        errors: ListBuffer[WaccError]
    ): Unit = {
        x.check(st, errors)
        y.check(st, errors)

        /** Ensure that expressions have checked successfully */
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        (x.typeId.get.getType(), y.typeId.get.getType()) match {
            case (IntType(), IntType()) => ()
            case (IntType(), _) =>
                errors += WaccError(
                  y.pos,
                  s"""expression ${y.repr()}'s type is incompatible for
					| '${op.symbol()}' (Expected: INT, Actual:
					| ${y.typeId.get.getType()}
					|)""".stripMargin.replaceAll("\n", "")
                )
            case _ =>
                errors += WaccError(
                  x.pos,
                  s"""expression ${x.repr()}'s type is incompatible for
					| '${op.symbol()}' (Expected: INT, Actual:
					| ${x.typeId.get.getType()}
					|)""".stripMargin.replaceAll("\n", "")
                )
        }
        op.typeId = Some(IntType())
    }

    /** Ordering binary operators
      *
      * Check types of ordering binary operations (GT, GTE, LT, LTE). Arguments
      * can be integers or characters, but both sides must be the same
      */
    def typeCheckOrderingBinOp(
        pos: (Int, Int),
        st: SymbolTable,
        op: BinaryOpNode,
        x: ExprNode,
        y: ExprNode,
        errors: ListBuffer[WaccError]
    ): Unit = {
        x.check(st, errors)
        y.check(st, errors)

        /** Ensure that expressions have checked successfully */
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        (x.typeId.get.getType(), y.typeId.get.getType()) match {
            case (IntType(), IntType())   => ()
            case (CharType(), CharType()) => ()
            case (CharType(), _) | (IntType(), _) =>
                errors += WaccError(
                  y.pos,
                  s"""expression ${y.repr()}'s type
				  | does not match expression ${x.repr()}'s type 
				  |for '${op.symbol()}' (Expected: ${x.typeId.get.getType()}
				  |, Actual: ${y.typeId.get.getType()}
				  |)""".stripMargin.replaceAll("\n", " ")
                )
            case _ =>
                errors += WaccError(
                  x.pos,
                  s"expression ${x.repr()}'s is incompatible for '${op.symbol()}' (Expected: INT or CHAR, Actual: ${x.typeId.get.getType()})"
                )
        }
        op.typeId = Some(BoolType())
    }

    /** Equality binary operators
      *
      * Check types of equality binary operations (Equal, NotEqual). Arguments
      * can be any, but both the left and right values must have same type.
      */
    def typeCheckEqualityBinOp(
        pos: (Int, Int),
        st: SymbolTable,
        op: BinaryOpNode,
        x: ExprNode,
        y: ExprNode,
        errors: ListBuffer[WaccError]
    ): Unit = {
        x.check(st, errors)
        y.check(st, errors)

        /** Ensure that expressions have checked successfully */
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        if (!lrTypeCheck(x.typeId.get.getType(), y.typeId.get.getType())) {
            errors += WaccError(
              x.pos,
              s"expression ${y.repr()}'s type does not match expression ${x
                  .repr()}'s type for '${op.symbol()}' (Expected: ${x.typeId.get
                  .getType()}, Actual: ${y.typeId.get.getType()})"
            )

        }
        op.typeId = Some(BoolType())
    }

    /** Logical binary operators
      *
      * Check types of logical binary operations (And, Or). Arguments can be
      * booleans.
      */
    def typeCheckLogicalBinOp(
        pos: (Int, Int),
        st: SymbolTable,
        op: BinaryOpNode,
        x: ExprNode,
        y: ExprNode,
        errors: ListBuffer[WaccError]
    ): Unit = {
        x.check(st, errors)
        y.check(st, errors)
        // Ensure that expressions have checked successfully
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        (x.typeId.get.getType(), y.typeId.get.getType()) match {
            case (BoolType(), BoolType()) => ()
            case (_, BoolType()) =>
                errors += WaccError(
                  y.pos,
                  s"expression ${y.repr()}'s type is incompatible for '${op
                      .symbol()}' (Expected: BOOL, Actual: ${y.typeId.get.getType()})"
                )
            case _ =>
                errors += WaccError(
                  x.pos,
                  s"expression ${x.repr()}'s type is incompatible for '${op
                      .symbol()}' (Expected: BOOL, Actual: ${x.typeId.get.getType()})"
                )
        }
        op.typeId = Some(BoolType())
    }
}
