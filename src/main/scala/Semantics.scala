import scala.collection.mutable.ListBuffer
object semantics {
    /* Check types of arithmetic binary operations (Mult, Div, Mod, Add, Sub)
        Arguments can be literals, variables, array elements. */
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
        // Ensure that expressions have checked successfully
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        x.typeId.get.getType() match {
            case IntType() => {
                y.typeId.get.getType() match {
                    case IntType() => {}
                    case _ =>
                        errors += WaccError(
                          pos,
                          s"expression ${y.repr()}'s type is incompatible for arithmetic operator (Expected: INT, Actual: ${y.typeId.get.getType()})"
                        )
                }
            }
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression ${x.repr()}'s type is incompatible for arithmetic operator (Expected: INT, Actual: ${x.typeId.get.getType()})"
                )
        }
        op.typeId = Some(IntType())
    }

    /* Check types of ordering binary operations (GT, GTE, LT, LTE) */
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
        // Ensure that expressions have checked successfully
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        x.typeId.get.getType() match {
            case IntType() => {
                y.typeId.get.getType() match {
                    case IntType() => {}
                    case _ =>
                        errors += WaccError(
                          pos,
                          s"expression ${y.repr()}'s type does not match expression ${x.repr()}'s type for ordering operator (Expected: ${x.typeId.get
                              .getType()}, Actual: ${y.typeId.get.getType()})"
                        )
                }
            }
            case CharType() => {
                y.typeId.get match {
                    case CharType() | Variable(CharType()) => {}
                    case _ =>
                        errors += WaccError(
                          pos,
                          s"expression ${y.repr()}'s type does not match expression ${x.repr()}'s type for ordering operator (Expected: ${x.typeId.get
                              .getType()}, Actual: ${y.typeId.get.getType()})"
                        )

                }
            }
            case _ => {
                errors += WaccError(
                  pos,
                  s"expression ${x.repr()}'s type is incompatible for ordering operator (Expected: INT or CHAR, Actual: ${x.typeId.get.getType()})"
                )
            }
        }
        op.typeId = Some(BoolType())
    }

    /* Check types of equality binary operations (Equal, NotEqual) */
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
        // Ensure that expressions have checked successfully
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        if (x.typeId.get.getType() != y.typeId.get.getType()) {
            errors += WaccError(
              pos,
              s"expression ${y.repr()}'s type does not match expression ${x
                  .repr()}'s type for equality operator (Expected: ${x.typeId.get
                  .getType()}, Actual: ${y.typeId.get.getType()})"
            )
        }
        op.typeId = Some(BoolType())
    }

    /* Check types of logical binary operations (And, Or) */
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
        x.typeId.get.getType() match {
            case BoolType() => {
                y.typeId.get.getType() match {
                    case BoolType() => {}
                    case _ =>
                        errors += WaccError(
                          pos,
                          s"expression ${y.repr()}'s type is incompatible for type check operator (Expected: BOOL, Actual: ${y.typeId.get.getType()})"
                        )
                }
            }
            case _ =>
                errors += WaccError(
                  pos,
                  s"expression ${x.repr()}'s type is incompatible for type check operator (Expected: BOOL, Actual: ${x.typeId.get.getType()})"
                )
        }
        op.typeId = Some(BoolType())
    }
}
