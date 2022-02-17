object transExpression {
    def apply(exprNode: ExprNode)(implicit
        collector: WaccBuffer
    ): List[Instruction] =
        // TODO: Each ExprNode match should return a list of instructions
        exprNode match {
            case Add(e1, e2) => { 
                transExpression(e1) ++ 
                List(PushInstr(Reg(0))) ++ 
                transExpression(e2) ++ 
                List(MoveInstr(Reg(1), RegOp(Reg(0))), 
                    PopInstr(Reg(0)), AddInstr(Reg(0), Reg(0), RegOp(Reg(1))))  
            }
            case IntLiterNode(n) => List(LoadImmInstr(Reg(0), n))
            case CharLiterNode(c) => List(MoveInstr(Reg(0), ImmOffset(c))) 
            case BoolLiterNode(true) => List(MoveInstr(Reg(0), ImmOffset(1))) 
            case BoolLiterNode(false) => List(MoveInstr(Reg(0), ImmOffset(0)))
            case _ => List[Instruction]().empty
        }
}
