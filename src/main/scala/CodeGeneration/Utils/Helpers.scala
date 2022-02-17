object Helpers {
    val WORD_SIZE = 4
    val BIT_SIZE = 1

    def getTypeSize(t: Identifier): Int = {
        t match {
            case BoolType() | CharType() => BIT_SIZE
            case Variable(t)             => getTypeSize(t)
            /** Functions are not applicable for stack frame sizes */
            case FunctionId(_, _, _) => 0
            case _                   => WORD_SIZE
        }
    }

    def getStringDirective(s: String, idx: Int): List[Instruction] = {

        List(
          Label(s"msg_$idx:"),
          Directive(s".word ${s.length()}"),
          Directive(s".ascii \"$s\"")
        )
    }
}
