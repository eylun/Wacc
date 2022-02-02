import parsley.Parsley, Parsley._

object SyntaxParser {
    import parsley.{Success, Failure}
    def main(args: Array[String]): Unit = {
        syntax.program.parse(args.head) match {
            case Success(x) => println(s"${args.head} is valid!")
            case Failure(err) => println(err)
        }
    }
}

object syntax {
    import parsley.implicits.character.{charLift, stringLift}
    import parsley.character.{anyChar, digit, endOfLine, noneOf}
    import parsley.combinator.{eof, many, manyUntil, optional, some}

    /* COMMENTS */

    // comment := '#' (any character except EOL)* <EOL>
    val comment = "#" ~> manyUntil(anyChar, endOfLine)


    /* LITERALS */

    // int-sign := '+' | '-'
    val intSign = "+" <|> "-"

    // int-liter := <int-sign>? <digit>+
    val intLiter = optional(intSign) <~> some(digit)

    // bool-liter := true | false
    val boolLiter = "true" <|> "false"

    // pair-liter := null
    val pairLiter = "null"

    // escaped-char := '0' | 'b' | 't' | 'n' | 'f' | 'r' | '"' | ''' | '\'
    val escapedChar = "0" <|> "b" <|> "t" <|> "n" <|> "f" <|> "r" <|> "\"" <|> "'" <|> "\\"

    // TODO: fix behavior of char and string literals - escaped '\'' below not working
    // character := (any ASCII char except '\', ''', '"') | <escaped-char>
    val character = noneOf('"', '\'', '\\') <|> "\\" <~> escapedChar

    // char-liter := ''' <character> '''
    val charLiter = "'" ~> character <~ "'"

    // string-liter := '"' <character>* '"'
    val stringLiter = "\"" ~> many(character) <~ "\""

    // ident := ('_' | 'a'-'z' | 'A'-'Z') ('_' | 'a'-'z' | 'A'-'Z' | '0'-'9')*
    // val ident = ???

    // TODO: remove - this is here for testing purposes
    val literal = intLiter <|> boolLiter <|> pairLiter <|> charLiter <|> stringLiter


    // TODO: change this at the end - currently set to check literals
    // program := 'begin' <func>* <stat> 'end'
    val program = literal <|> comment <~ eof
}
