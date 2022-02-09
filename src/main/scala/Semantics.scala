import scala.collection.mutable.ListBuffer
object semantics {
    /* Check types of arithmetic binary operations (Mult, Div, Mod, Add, Sub)
        Arguments can be literals, variables, array elements. */
    def typeCheckArithmeticBinOp(
        st: SymbolTable,
        op: BinaryOpNode,
        x: ExprNode,
        y: ExprNode,
        errors: ListBuffer[String]
    ): Unit = {
        x.check(st, errors)
        y.check(st, errors)
        // Ensure that expressions have checked successfully
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        x.typeId.get match {
            case IntType() | Variable(IntType()) => {
                y.typeId.get match {
                    case IntType() | Variable(IntType()) => {
                        op.typeId = Some(IntType())
                    }
                    case _ =>
                        errors +=
                            "incompatible argument type for arithmetic operator"
                }
            }
            case _ =>
                errors += "incompatible argument type for arithmetic operator"
        }
    }

    /* Check types of ordering binary operations (GT, GTE, LT, LTE) */
    def typeCheckOrderingBinOp(
        st: SymbolTable,
        op: BinaryOpNode,
        x: ExprNode,
        y: ExprNode,
        errors: ListBuffer[String]
    ): Unit = {
        x.check(st, errors)
        y.check(st, errors)
        // Ensure that expressions have checked successfully
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        x.typeId.get match {
            case IntType() | Variable(IntType()) => {
                y.typeId.get match {
                    case IntType() | Variable(IntType()) => {}
                    case _ =>
                        errors +=
                            "non-matching argument types for ordering operator"
                }
            }
            case CharType() | Variable(CharType()) => {
                y.typeId.get match {
                    case CharType() | Variable(CharType()) => {}
                    case _ =>
                        errors +=
                            "non-matching argument types for ordering operator"

                }
            }
            case _ => {
                errors += "incompatible argument type for ordering operator"
            }
        }
        op.typeId = Some(BoolType())
    }

    /* Check types of equality binary operations (Equal, NotEqual) */
    def typeCheckEqualityBinOp(
        st: SymbolTable,
        op: BinaryOpNode,
        x: ExprNode,
        y: ExprNode,
        errors: ListBuffer[String]
    ): Unit = {
        x.check(st, errors)
        y.check(st, errors)
        // Ensure that expressions have checked successfully
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        x.typeId.get match {
            case Variable(tx) => {
                y.typeId.get match {
                    case Variable(ty) => {
                        if (tx != ty)
                            errors +=
                                "non-matching argument types for equality operator"
                    }
                    case _ => {
                        if (tx != y.typeId.get)
                            errors +=
                                "non-matching argument types for equality operator"

                    }
                }
            }
            case _ => {
                y.typeId.get match {
                    case Variable(ty) => {
                        if (x.typeId.get != ty)
                            errors +=
                                "non-matching argument types for equality operator"
                    }
                    case _ => {
                        if (x.typeId.get != y.typeId.get)
                            errors +=
                                "non-matching argument types for equality operator"
                    }
                }
            }
        }
        op.typeId = Some(BoolType())
    }

    /* Check types of logical binary operations (And, Or) */
    def typeCheckLogicalBinOp(
        st: SymbolTable,
        op: BinaryOpNode,
        x: ExprNode,
        y: ExprNode,
        errors: ListBuffer[String]
    ): Unit = {
        x.check(st, errors)
        y.check(st, errors)
        // Ensure that expressions have checked successfully
        if (x.typeId.isEmpty || y.typeId.isEmpty) return ()
        x.typeId.get match {
            case BoolType() | Variable(BoolType()) => {
                y.typeId.get match {
                    case BoolType() | Variable(BoolType()) => {
                        op.typeId = Some(BoolType())
                    }
                    case _ =>
                        errors +=
                            "incompatible argument type for arithmetic operator"
                }
            }
            case _ =>
                errors += "incompatible argument type for arithmetic operator"
        }
    }
}
