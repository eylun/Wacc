object semantics {
    /* Check types of arithmetic binary operations (Mult, Div, Mod, Add, Sub)
        Arguments can be literals, variables, array elements. */
    def typeCheckArithmeticBinOp(st: SymbolTable, op: BinaryOpNode,
                            x: ExprNode, y: ExprNode): Unit = {
        x.check(st)
        y.check(st)

        x.typeId.get match {
            case IntType() | Variable(IntType()) => {
                y.typeId.get match {
                    case IntType() | Variable(IntType()) 
                        => { op.typeId = Some(IntType()) }
                    case _
                        => println("incompatible argument type for arithmetic operator")
                }
            }
            case _        => println("incompatible argument type for arithmetic operator")    
        }
    }

    /* Check types of ordering binary operations (GT, GTE, LT, LTE) */
    def typeCheckOrderingBinOp(st: SymbolTable, op: BinaryOpNode,
                                x: ExprNode, y: ExprNode): Unit = {
        x.check(st)
        y.check(st)

        x.typeId.get match {
            case IntType() | Variable(IntType()) => {
                y.typeId.get match {
                    case IntType() | Variable(IntType()) => {}
                    case _ => println("non-matching argument types for ordering operator")
                }
            }
            case CharType() | Variable(CharType()) => {
                y.typeId.get match {
                    case CharType() | Variable(CharType()) => {}
                    case _ => println("non-matching argument types for ordering operator")
                }
            }
            case _ => {
                println("incompatible argument type for ordering operator")
            }
        }
        op.typeId = Some(BoolType())
    }
    
    /* Check types of equality binary operations (Equal, NotEqual) */
    def typeCheckEqualityBinOp(st: SymbolTable, op: BinaryOpNode,
                                x: ExprNode, y: ExprNode): Unit = {
        x.check(st)
        y.check(st)

        x.typeId.get match {
            case Variable(tx) => {
                y.typeId.get match {
                    case Variable(ty) => {
                        if (tx != ty) 
                            println("non-matching argument types for equality operator")
                    }
                    case _ => {
                        if (tx != y.typeId.get) 
                            println("non-matching argument types for equality operator")
                    }
                }
            }
            case _ => {
                y.typeId.get match {
                    case Variable(ty) => {
                        if (x.typeId.get != ty) 
                            println("non-matching argument types for equality operator")
                    }
                    case _ => {
                        if (x.typeId.get != y.typeId.get) 
                            println("non-matching argument types for equality operator")
                    }
                }
            }
        }
        op.typeId = Some(BoolType())
    }

    /* Check types of logical binary operations (And, Or) */
    def typeCheckLogicalBinOp(st: SymbolTable, op: BinaryOpNode,
                                x: ExprNode, y: ExprNode): Unit = {
        x.check(st)
        y.check(st)

        x.typeId.get match {
            case BoolType() | Variable(BoolType()) => {
                y.typeId.get match {
                    case BoolType() | Variable(BoolType()) 
                        => { op.typeId = Some(BoolType()) }
                    case _
                        => println("incompatible argument type for arithmetic operator")
                }
            }
            case _        => println("incompatible argument type for arithmetic operator")    
        }                           
    }
}
