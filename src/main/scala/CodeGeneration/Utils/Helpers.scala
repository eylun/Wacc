object Helpers {
    val WORD_SIZE = 4
    val BIT_SIZE = 1

    def getTypeSize(t: Identifier): Int = {
        t match {
            case BoolType() | CharType()           => BIT_SIZE
            case Variable(_) | FunctionId(_, _, _) => 0
            case _                                 => WORD_SIZE
        }
    }
}
