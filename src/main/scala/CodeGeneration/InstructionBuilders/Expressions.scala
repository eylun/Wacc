import scala.collection.mutable

object transExpression {
    def apply(exprNode: ExprNode)(implicit
        collector: mutable.ListBuffer[Instruction]
    ): List[Instruction] =
        // TODO: Each ExprNode match should return a list of instructions
        exprNode match {
            case _ => List[Instruction]().empty
        }
}
