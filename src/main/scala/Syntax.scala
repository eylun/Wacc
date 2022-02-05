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
}

/* Lexer */
object lexer {
    import parsley.character.{digit, isWhitespace, alphaNum, noneOf, string}
    import parsley.token.{LanguageDef, Lexer, Parser, Predicate}
    import parsley.implicits.character.{charLift, stringLift}
    import parsley.combinator.{eof, many, manyUntil, optional, some}

    val lang = LanguageDef.plain.copy(
      commentLine = "#",
      space = Predicate(isWhitespace),
      identStart = Predicate(_.isLetterOrDigit),
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

    val character: Parsley[Char] =
        noneOf('"', '\'', '\\') <|> ("\\" ~> escapedChar)

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
    import lexer.{fully, exprAtoms, number, identifier}
    import lexer.implicits.implicitLexeme
    import parsley.combinator.{eof, many, manyUntil, optional, some, sepBy, sepBy1}
    import parsley.expr.{precedence, Ops, InfixL, Prefix}

    /* TODO: change this at the end - currently set to check multiple
       expressions separareted by semicolons */

    // program := 'begin' <func>* <stat> 'end'
    lazy val program = "begin" ~> sepBy(stat, ";") <~ "end"

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
    lazy val newPair = NewPairNode.lift("newpair" *> "(" *> expr <* ",", expr <* ")")

    // call := ‘call’ ⟨ident⟩ ‘(’ ⟨arg-list⟩? ‘)’
    lazy val call = CallNode.lift("call" *> identifier, "(" *> exprArgList <* ")")

    // array-liter := ‘[’ ( ⟨expr ⟩ (‘,’ ⟨expr ⟩)* )? ‘]’
    // ***Note: difference between option vs. optional?
    lazy val arrayLiter = ArrayLiterNode.lift("[" *> sepBy1(expr, ",") <* "]")   


    // assign-rhs := expr <|> array-liter <|> 'newpair' '(' expr ',' expr ')' <|> pairElem 
    //               <|> 'call' ident '(' arg-list? ')'
    lazy val assignRHS = expr <|> arrayLiter <|> newPair <|> pairElem <|> call

    
    lazy val skipStat = "skip" #> SkipNode()
    lazy val lrAssignStat = LRAssignNode.lift(assignLHS, "=" *> assignRHS)
    lazy val readStat = ReadNode.lift("read" *> assignLHS)
    lazy val freeStat = FreeNode.lift("free" *> expr)
    lazy val returnStat = ReturnNode.lift("return" *> expr)
    lazy val exitStat = ExitNode.lift("exit" *> expr)
    lazy val printStat = PrintNode.lift("print" *> expr)
    lazy val printlnStat = PrintlnNode.lift("println" *> expr)
    
    //if-else-stat := ‘if’ ⟨expr ⟩ ‘then’ ⟨stat ⟩ ‘else’ ⟨stat ⟩ ‘fi’
    lazy val ifThenElseStat: Parsley[StatNode] = 
        IfThenElseNode.lift("if" *> expr, "then" *> stat, "else" *> stat <* "fi")

    //while-do-stat := ‘while’ ⟨expr ⟩ ‘do’ ⟨stat ⟩ ‘done’
    lazy val whileDoStat =
        WhileDoNode.lift("while" *> expr, "do" *> stat <* "done")   


    // begin-end-stat := ‘begin’ ⟨stat ⟩ ‘end’
    lazy val beginEndStat =
        BeginEndNode.lift("begin" *> stat <* "end")   

    // stat := 'skip' | ⟨type ⟩ ⟨ident ⟩ ‘=’ ⟨assign-rhs ⟩ | ⟨assign-lhs ⟩ ‘=’ ⟨assign-rhs ⟩ | ‘read’ ⟨assign-lhs ⟩
    //  | ‘free’ ⟨expr ⟩ | ‘return’ ⟨expr ⟩ | ‘exit’ ⟨expr ⟩ | ‘print’ ⟨expr ⟩ | ‘println’ ⟨expr ⟩
    //  | ‘if’ ⟨expr ⟩ ‘then’ ⟨stat ⟩ ‘else’ ⟨stat ⟩ ‘fi’ | ‘while’ ⟨expr ⟩ ‘do’ ⟨stat ⟩ ‘done’
    //  | ‘begin’ ⟨stat ⟩ ‘end’ | ⟨stat ⟩ ‘;’ ⟨stat ⟩
    lazy val stat: Parsley[StatNode] = 
      skipStat <|> lrAssignStat <|> readStat <|> freeStat <|> returnStat <|> exitStat <|> printStat <|> printlnStat <|> 
      ifThenElseStat <|> whileDoStat <|> beginEndStat




}   


