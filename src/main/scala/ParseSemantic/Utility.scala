object Utility {
    def lrTypeCheck(l: Type, r: Type): Boolean = {
        // println("hahaha ", l, r)
        (l, r) match {
            case (_, AnyType()) => true
            case (ArrayType(t1, n1), ArrayType(t2, n2)) =>
                n1 == n2 && lrTypeCheck(t1, t2)
            case (PairType(_, _), NullPairType()) |
                (PairType(_, _), NestedPairType()) |
                (NullPairType(), PairType(_, _)) |
                (NestedPairType(), PairType(_, _)) |
                (NullPairType(), NestedPairType()) |
                (NestedPairType(), NullPairType()) =>
                true
            case (PairType(pair1L, pair1R), PairType(pair2L, pair2R)) =>
                lrTypeCheck(pair1L, pair2L) && lrTypeCheck(pair1R, pair2R)
            case (l, r) => l == r
        }
    }

    val keywords = Set(
      "begin",
      "end",
      "is",
      "skip",
      "read",
      "free",
      "return",
      "exit",
      "print",
      "println",
      "if",
      "then",
      "else",
      "fi",
      "while",
      "do",
      "done",
      "newpair",
      "call",
      "fst",
      "snd",
      "int",
      "bool",
      "chr",
      "string",
      "pair",
      "len",
      "ord",
      "chr",
      "true",
      "false",
      "null"
    )

    val operators = Set(
      "*",
      "/",
      "%",
      "+",
      "-",
      ">",
      ">=",
      "<",
      "<=",
      "==",
      "!=",
      "&&",
      "||"
    )

    val escapedChars = Map(
      '0' -> '\u0000',
      'b' -> '\b',
      't' -> '\t',
      'n' -> '\n',
      'f' -> '\f',
      'r' -> '\r',
      '"' -> '\"',
      '\'' -> '\'',
      '\\' -> '\\'
    )
}
