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
    import parsley.character.{
        digit,
        isWhitespace,
        alphaNum,
        noneOf,
        string,
        letter
    }
    import parsley.token.{LanguageDef, Lexer, Parser, Predicate}
    import parsley.implicits.character.{charLift, stringLift}
    import parsley.combinator.{eof, many, manyUntil, optional, some}
    import parsley.errors.combinator.ErrorMethods

    val lang = LanguageDef.plain.copy(
      commentLine = "#",
      space = Predicate(isWhitespace),
      identStart = Predicate(c => (c == '_' || c.isLetter)),
      identLetter = Predicate(c => (c == '_' || c.isLetterOrDigit)),
      keywords = utility.keywords,
      operators = utility.operators
    )

    val lexer = new Lexer(lang)

    def fully[A](p: => Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof

    val ident = IdentNode(lexer.identifier)
    val number =
        lexer
            .lexeme(
              digit
                  .label("end of number")
                  .foldLeft1[Long](0)((n, d) => n * 10 + d.asDigit)
            )
            .label("number")

    // escaped-char := '0' | 'b' | 't' | 'n' | 'f' | 'r' | '"' | ''' | '\'
    val escapedChar =
        '0' <|> 'b' <|> 't' <|> 'n' <|> 'f' <|> 'r' <|> '\"' <|> '\'' <|> '\\'

    val esc =
        ((c: Char) => utility.escapedChars.apply(c)) <#> escapedChar
            .label(
              "end of escape sequence"
            )
            .explain(
              "valid escape sequences include \\0, \\b, \\t, \\n, \\f, \\r, \\\", \\\', \\\\"
            )

    val character: Parsley[Char] =
        noneOf('"', '\'', '\\') <|> ("\\" *> esc)

    // int-sign := '+' | '-'
    val intSign = "+" #> identity[Long] _ <|> "-" #> ((x: Long) => -x)

    /* LITERALS */

    // char-liter := ''' <character> '''
    val charLiter: Parsley[ExprNode] =
        CharLiterNode(
          lexer.lexeme("'" ~> character.label("character") <~ "'")
        )

    // int-liter := <int-sign>? <digit>+
    val intLiter: Parsley[ExprNode] =
        IntLiterNode(
          (intSign <*> number <|> number)
              .collectMsg("Integer Overflow") {
                  case x if x.toInt == x => x.toInt
              }
              .label("integer literal")
        )

    // TODO: Try to use implicits to not use 'lex.keyword' everytime
    // bool-liter := true | false
    val boolLiter: Parsley[ExprNode] =
        BoolLiterNode(
          lexer.keyword("true") #> true <|> lexer.keyword("false") #> false
        )

    // pair-liter := null
    val pairLiter: Parsley[ExprNode] = lexer.keyword("null") *> PairLiterNode()

    // string-liter := '"' <character>* '"'
    val stringLiter =
        StringLiterNode(
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
        (intLiter <|> boolLiter <|> charLiter <|> stringLiter <|> pairLiter <|>
            ident)
            .label("expression atom")
            .explain(
              "an expression atom is an identifier, integer, boolean, character, string or a pair literal"
            )

    /* TYPES */
    // base-type := 'int' | 'bool' | 'char' | 'string'
    lazy val intType = IntTypeNode <# lexer.keyword("int")
    lazy val boolType = BoolTypeNode <# lexer.keyword("bool")
    lazy val charType = CharTypeNode <# lexer.keyword("char")
    lazy val stringType = StringTypeNode <# lexer.keyword("string")
    lazy val baseType: Parsley[BaseTypeNode] =
        intType <|> boolType <|> charType <|> stringType
    val pairBaseType = PairElemTypePairNode <# lexer.keyword("pair")

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
    import lexer.{baseType, pairBaseType, fully, exprAtoms, number, ident}
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
    import parsley.errors.combinator.ErrorMethods

    /* TODO: change this at the end - currently set to check multiple
       expressions separareted by semicolons */

    // program := 'begin' <func>* <stat> 'end'
    lazy val program = ProgramNode(
      "begin".label("beginning of program") ~> many(func).label(
        "function declarations"
      ),
      stat <~ "end".label("end of program")
    )

    val parse = fully(program)

    /* FUNCTIONS */
    lazy val func: Parsley[FuncNode] =
        attempt {
            FuncNode(
              anyType,
              ident,
              "(" ~> sepBy(param, ",") <~ ")",
              "is" ~> stat
                  .collectMsg(
                    "Missing return or exit statement on a path"
                  )(verifyFuncExit)
                  .collectMsg(
                    "Return Statement not at end of statement block"
                  )(verifyCleanExit) <~ "end"
            )
        }

    lazy val param: Parsley[ParamNode] = ParamNode(anyType, ident)
    lazy val verifyFuncExit: PartialFunction[StatNode, StatNode] = {
        case x if canExit(x) => x
    }
    lazy val verifyCleanExit: PartialFunction[StatNode, StatNode] = {
        case x if cleanExit(x) => x
    }
    def canExit(base: StatNode): Boolean = {
        base match {
            case ReturnNode(_) | ExitNode(_) => true
            case IfThenElseNode(_, s1, s2)   => canExit(s1) && canExit(s2)
            case StatListNode(s)             => s.exists(canExit)
            case _                           => false
        }
    }

    def cleanExit(base: StatNode): Boolean = {
        base match {
            case ReturnNode(_) | ExitNode(_) => true
            case IfThenElseNode(_, s1, s2)   => cleanExit(s1) && cleanExit(s2)
            case StatListNode(s) =>
                s.zipWithIndex.foreach {
                    case (ReturnNode(_) | ExitNode(_), n)
                        if n == s.length - 1 =>
                        return true
                    case (x: IfThenElseNode, _) if cleanExit(x) => return true
                    case _                                      => ()
                }
                false
            case _ => false
        }
    }
    /* EXPRESSIONS */
    // expr := literal <|> identifier <|> array-elem <|> unary op
    // 		<|> bin op <|> paren
    // unary-oper
    lazy val expr: Parsley[ExprNode] =
        precedence[ExprNode](
          ("(" *> expr <* ")").label("bracketed expression")
              <|> attempt(arrayElem)
              <|> exprAtoms
        )(
          Ops(Prefix)(
            Not <# "!",
            Neg <# attempt("-" <~ notFollowedBy(number)),
            Len <# "len",
            Ord <# "ord",
            Chr <# "chr"
          ),
          Ops(InfixL)(Mult <# "*", Div <# "/", Mod <# "%"),
          Ops(InfixL)(Add <# "+", Sub <# "-"),
          Ops(InfixL)(GTE <# ">=", LTE <# "<=", LT <# "<", GT <# ">"),
          Ops(InfixL)(Equal <# "==", NotEqual <# "!="),
          Ops(InfixL)(And <# "&&"),
          Ops(InfixL)(Or <# "||")
        )
    // array-elem := identifier ('[' <expr> ']')+
    lazy val arrayElem =
        ArrayElemNode(ident, some("[" *> expr <* "]"))
            .label("array element")
            .explain(
              "array elements are structured as <identifier> [<expression>]"
            )

    // pair-elem := 'fst' expr <|> 'snd' expr
    lazy val firstPairElem = FirstPairElemNode("fst" *> expr)
    lazy val secondPairElem = SecondPairElemNode("snd" *> expr)

    lazy val pairElem = firstPairElem <|> secondPairElem
    /* ASSIGNMENTS */
    // assignLHS := ident <|> array-elem <|> pair-elem
    lazy val assignLHS = attempt(arrayElem) <|> ident <|> pairElem

    // arg-list := expr (',' expr )*
    lazy val exprArgList: Parsley[List[ExprNode]] = sepBy(expr, ",")

    // Variables newPair, arrayLiter and call are for assign-rhs parsing
    // newPair := 'newpair''(' expr ',' expr ')'
    lazy val newPair =
        NewPairNode("newpair" *> "(" *> expr <* ",", expr <* ")")

    // call := ‘call’ ⟨ident⟩ ‘(’ ⟨arg-list⟩? ‘)’
    lazy val call =
        CallNode("call" *> ident, "(" *> exprArgList <* ")")

    // array-liter := ‘[’ ( ⟨expr ⟩ (‘,’ ⟨expr ⟩)* )? ‘]’
    // ***Note: difference between option vs. optional?
    lazy val arrayLiter = ArrayLiterNode("[" *> sepBy(expr, ",") <* "]")

    // assign-rhs := expr <|> array-liter <|> 'newpair' '(' expr ',' expr ')'
    // <|> pairElem <|> 'call' ident '(' arg-list? ')'
    lazy val assignRHS = expr <|> arrayLiter <|> newPair <|> pairElem <|> call

    /* STATEMENTS */
    lazy val skipStat = SkipNode <# "skip"
    lazy val newAssignStat =
        NewAssignNode(anyType, ident, "=" *> assignRHS)
    lazy val lrAssignStat = LRAssignNode(assignLHS, "=" *> assignRHS)
    lazy val readStat = ReadNode("read" *> assignLHS)
    lazy val freeStat = FreeNode("free" *> expr)
    lazy val returnStat = ReturnNode("return" *> expr)
    lazy val exitStat = ExitNode("exit" *> expr)
    lazy val printStat = PrintNode("print" *> expr)
    lazy val printlnStat = PrintlnNode("println" *> expr)

    //if-else-stat := ‘if’ ⟨expr ⟩ ‘then’ ⟨stat ⟩ ‘else’ ⟨stat ⟩ ‘fi’
    lazy val ifThenElseStat: Parsley[StatNode] =
        IfThenElseNode(
          "if" *> expr,
          "then" *> stat,
          "else" *> stat <* "fi"
        )

    //while-do-stat := ‘while’ ⟨expr ⟩ ‘do’ ⟨stat ⟩ ‘done’
    lazy val whileDoStat =
        WhileDoNode("while" *> expr, "do" *> stat <* "done")

    // begin-end-stat := ‘begin’ ⟨stat ⟩ ‘end’
    lazy val beginEndStat =
        BeginEndNode("begin" *> stat <* "end")

    lazy val statList: Parsley[StatNode] =
        StatListNode(sepBy1(statAtoms, ";"))
    // stat := 'skip' | ⟨type ⟩ ⟨ident ⟩ ‘=’ ⟨assign-rhs ⟩
    // 	| ⟨assign-lhs ⟩ ‘=’ ⟨assign-rhs ⟩ | ‘read’ ⟨assign-lhs ⟩
    //  | ‘free’ ⟨expr ⟩ | ‘return’ ⟨expr ⟩ | ‘exit’ ⟨expr ⟩
    //  | ‘print’ ⟨expr ⟩ | ‘println’ ⟨expr ⟩
    //  | ‘if’ ⟨expr ⟩ ‘then’ ⟨stat ⟩ ‘else’ ⟨stat ⟩ ‘fi’
    //  | ‘while’ ⟨expr ⟩ ‘do’ ⟨stat ⟩ ‘done’
    //  | ‘begin’ ⟨stat ⟩ ‘end’ | ⟨stat ⟩ ‘;’ ⟨stat ⟩
    lazy val statAtoms: Parsley[StatNode] =
        skipStat <|> newAssignStat <|> lrAssignStat <|> readStat <|>
            freeStat <|> returnStat <|> exitStat <|> printStat <|>
            printlnStat <|> ifThenElseStat <|> whileDoStat <|> beginEndStat

    lazy val stat: Parsley[StatNode] =
        statList <|> statAtoms <* notFollowedBy(";")

    /* TYPES */
    // type := <base-type> | <array-type> | <pair-type>
    lazy val anyType: Parsley[TypeNode] =
        attempt(arrayType) <|> pairType <|> baseType

    // array-type := <type> '[' ']'
    lazy val arrayType: Parsley[ArrayTypeNode] =
        chain.postfix1(
          ArrayTypeNode(pairType <|> baseType, 0),
          ArrayTypeNode("[]")
        )

    // pair-elem-type := <base-type> | <array-type> | 'pair'
    lazy val pairElemType: Parsley[PairElemTypeNode] =
        attempt(arrayType) <|> baseType <|> pairBaseType

    // pair-type := 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
    lazy val pairType: Parsley[PairTypeNode] = PairTypeNode(
      "pair" *> "(" *> pairElemType <* ",",
      pairElemType <* ")"
    )
}
