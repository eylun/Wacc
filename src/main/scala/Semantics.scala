object semantics {
    /* Check types of arithmetic binary operations (Mult, Div, Mod, Add, Sub)
        Arguments can be literals, variables, array elements. */
    def typeCheckArithmeticBinOp(st: SymbolTable, op: BinaryOpNode,
                            x: ExprNode, y: ExprNode): Unit = {
        x.check(st)
        y.check(st)

        x.typeId.get match {
            case IntType() | Variable(IntType()) | FunctionId(IntType(), _, _) => {
                y.typeId.get match {
                    case IntType() | Variable(IntType()) | FunctionId(IntType(), _, _) 
                        => { op.typeId = Some(IntType()) }
                    case _
                        => println("incompatible argument type for operator '*'")
                }
            }
            case _        => println("incompatible argument type for operator '*'")    
        }
    }

    /* Check types of ordering binary operations (GT, GTE, LT, LTE) */
    def typeCheckOrderingBinOp(st: SymbolTable, op: BinaryOpNode,
                                x: ExprNode, y: ExprNode): Unit = {
        x.check(st)
        y.check(st)
        // TODO check x, y and update op.typeId                                
    }
    
    /* Check types of equality binary operations (Equal, NotEqual) */
    def typeCheckEqualityBinOp(st: SymbolTable, op: BinaryOpNode,
                                x: ExprNode, y: ExprNode): Unit = {
        x.check(st)
        y.check(st)
        // TODO check x, y and update op.typeId                                
    }

    /* Check types of logical binary operations (And, Or) */
    def typeCheckLogicalBinOp(st: SymbolTable, op: BinaryOpNode,
                                x: ExprNode, y: ExprNode): Unit = {
        x.check(st)
        y.check(st)
        // TODO check x, y and update op.typeId                                
    }
}
