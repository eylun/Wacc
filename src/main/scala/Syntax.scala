import parsley.Parsley, Parsley._
import parsley.implicits.lift.{Lift0, Lift1, Lift2, Lift3, Lift4}
import scala.language.implicitConversions

object utility {
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

/* Lexer */
object lexer {
    import parsley.character.{digit, isWhitespace, alphaNum, noneOf, string, letter}
    import parsley.token.{LanguageDef, Lexer, Parser, Predicate}
    import parsley.implicits.character.{charLift, stringLift}
    import parsley.combinator.{eof, many, manyUntil, optional, some}

    val lang = LanguageDef.plain.copy(
      commentLine = "#",
      space = Predicate(isWhitespace),
      identStart = Parser('_' <|> letter),
      identLetter = Parser("_" <|> alphaNum),
      keywords = utility.keywords,
      operators = utility.operators
    )

    val lexer = new Lexer(lang)

    def fully[A](p: => Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof

    val identifier = IdentNode.lift(lexer.identifier)
    val number =
        lexer.lexeme(digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit))

    // escaped-char := '0' | 'b' | 't' | 'n' | 'f' | 'r' | '"' | ''' | '\'
    val escapedChar =
        '0' <|> 'b' <|> 't' <|> 'n' <|> 'f' <|> 'r' <|> '\"' <|> '\'' <|> '\\'

    val esc = ((c: Char) => utility.escapedChars.apply(c)) <#> escapedChar

    val character: Parsley[Char] =
        noneOf('"', '\'', '\\') <|> ("\\" *> esc)

    // int-sign := '+' | '-'
    val intSign = "+" #> identity[Int] _ <|> "-" #> ((x: Int) => -x)

    /* LITERALS */

    // char-liter := ''' <character> '''
    val charLiter =
        CharLiterNode.lift(
          lexer.lexeme("'" ~> character <~ "'")
        )

    // int-liter := <int-sign>? <digit>+
    val intLiter: Parsley[ExprNode] =
        IntLiterNode.lift(intSign <*> number <|> number)

    // TODO: Try to use implicits to not use 'lex.keyword' everytime
    // bool-liter := true | false
    val boolLiter =
        BoolLiterNode.lift(
          lexer.keyword("true") #> true <|> lexer.keyword("false") #> false
        )

    // pair-liter := null
    val pairLiter = lexer.keyword("null") #> PairLiterNode()

    // string-liter := '"' <character>* '"'
    val stringLiter =
        StringLiterNode.lift(
          (lexer
              .lexeme(
                "\"" ~> many(
                  character
                ) <~ "\""
              ))
              .map(s => s.mkString)
        )

    // expression atoms
    val exprAtoms: Parsley[ExprNode] =
        intLiter <|> boolLiter <|> charLiter <|> stringLiter <|> pairLiter <|>
            identifier

    /* TYPES */
    // base-type := 'int' | 'bool' | 'char' | 'string'
    lazy val intType = lexer.keyword("int") #> IntTypeNode()
    lazy val boolType =
        lexer.keyword("bool") #> BoolTypeNode()
    lazy val charType =
        lexer.keyword("char") #> CharTypeNode()
    lazy val stringType =
        lexer.keyword("string") #> StringTypeNode()
    lazy val baseType: Parsley[BaseTypeNode] =
        intType <|> boolType <|> charType <|> stringType
    val pairBaseType =
        lexer.keyword("pair") #> PairElemTypePairNode()

    object implicits {
        implicit def implicitLexeme(s: String): Parsley[Unit] = {
            if (lang.keywords(s)) lexer.keyword(s)
            else if (lang.operators(s)) lexer.maxOp(s)
            else void(lexer.symbol_(s))
        }
    }
}

/* Syntax Parser */
object syntax {
    import lexer.{baseType, pairBaseType, fully, exprAtoms, number, identifier}
    import lexer.implicits.implicitLexeme
    import parsley.debug.{DebugCombinators, FullBreak}
    import parsley.combinator.{
        eof,
        many,
        manyUntil,
        optional,
        some,
        sepBy,
        sepBy1
    }
    import parsley.expr.{precedence, Ops, InfixL, Prefix, Postfix, chain}

    /* TODO: change this at the end - currently set to check multiple
       expressions separareted by semicolons */

    // program := 'begin' <func>* <stat> 'end'
    lazy val program = "begin" ~> sepBy(anyType, ";") <~ "end"

    val parse = fully(program)

    // expr := literal <|> identifier <|> array-elem <|> unary op
    // 		<|> bin op <|> paren
    // unary-oper
    lazy val expr: Parsley[ExprNode] =
        precedence[ExprNode]("(" *> expr <* ")" <|> exprAtoms <|> arrayElem)(
          Ops(Prefix)(
            "!" #> Not,
            attempt("-" <~ notFollowedBy(number)) #> Neg,
            "len" #> Len,
            "ord" #> Ord,
            "chr" #> Chr
          ),
          Ops(InfixL)("*" #> Mult, "/" #> Div, "%" #> Mod),
          Ops(InfixL)("+" #> Add, "-" #> Sub),
          Ops(InfixL)(">=" #> GTE, "<=" #> LTE, "<" #> LT, ">" #> GT),
          Ops(InfixL)("==" #> Equal, "!=" #> NotEqual),
          Ops(InfixL)("&&" #> And),
          Ops(InfixL)("||" #> Or)
        )

    // array-elem := identifier ('[' <expr> ']')+
    lazy val arrayElem =
        ArrayElemNode.lift(identifier, some("[" *> expr <* "]"))

    // pair-elem := 'fst' expr <|> 'snd' expr
    lazy val firstPairElem = FirstPairElemNode.lift("fst" *> expr)
    lazy val secondPairElem = SecondPairElemNode.lift("snd" *> expr)

    lazy val pairElem = firstPairElem <|> secondPairElem

    // assignLHS := ident <|> array-elem <|> pair-elem
    lazy val assignLHS = identifier <|> arrayElem <|> pairElem

    // arg-list := expr (',' expr )*
    lazy val exprArgList: Parsley[List[ExprNode]] = sepBy(expr, ",")

    // Variables newPair, arrayLiter and call are for assign-rhs parsing
    // newPair := 'newpair''(' expr ',' expr ')'
    lazy val newPair =
        NewPairNode.lift("newpair" *> "(" *> expr <* ",", expr <* ")")

    // call := ‘call’ ⟨ident⟩ ‘(’ ⟨arg-list⟩? ‘)’
    lazy val call =
        CallNode.lift("call" *> identifier, "(" *> exprArgList <* ")")

    // array-liter := ‘[’ ( ⟨expr ⟩ (‘,’ ⟨expr ⟩)* )? ‘]’
    // ***Note: difference between option vs. optional?
    lazy val arrayLiter = ArrayLiterNode.lift("[" *> sepBy1(expr, ",") <* "]")

    // assign-rhs := expr <|> array-liter <|> 'newpair' '(' expr ',' expr ')' <|> pairElem
    //               <|> 'call' ident '(' arg-list? ')'
    lazy val assignRHS = expr <|> arrayLiter <|> newPair <|> pairElem <|> call

    /* TYPES */
    // type := <base-type> | <array-type> | <pair-type>
    lazy val anyType: Parsley[TypeNode] = arrayType <|> pairType <|> baseType

    // array-type := <type> '[' ']'
    lazy val arrayType: Parsley[TypeNode] =
        precedence[TypeNode](pairType <|> baseType)(
          (Ops(Postfix)("[]" #> ((t: TypeNode) => {
              t match {
                  case ArrayTypeNode(child, n) => ArrayTypeNode(child, n + 1)
                  case _                       => ArrayTypeNode(t, 1)
              }
          })))
        )

    // pair-elem-type := <base-type> | <array-type> | 'pair'
    lazy val pairElemType: Parsley[PairElemTypeNode] =
        attempt(arrayType.cast[ArrayTypeNode]) <|> baseType <|> pairBaseType

    // pair-type := 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
    lazy val pairType = PairTypeNode.lift(
      "pair" *> "(" *> pairElemType <* ",",
      pairElemType <* ")"
    )
}
