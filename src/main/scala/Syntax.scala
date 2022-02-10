import parsley.Parsley, Parsley._
import parsley.implicits.lift.{Lift0, Lift1, Lift2, Lift3, Lift4}
import scala.language.implicitConversions
import parsley.errors.combinator.{
    fail => pfail,
    unexpected,
    amend,
    entrench,
    ErrorMethods
}

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

    // def forceKeywordCheck(p: => Parsley[Unit]): Unit = p
    //     .map(q => lexer.keyword(q))
    //     .unexpected(s => s"unexpected keyword $s}")

    val ident = IdentNode(lexer.identifier.filterOut {
        case v if lang.keywords(v) =>
            s"keyword $v may not be used as an identifier"
    }).label("identifier")

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
        ('0' <|> 'b' <|> 't' <|> 'n' <|> 'f' <|> 'r' <|> '\"' <|> '\'' <|> '\\')
            .label(
              "end of escape sequence"
            )
            .explain(
              "- escape sequences include: \\0, \\b, \\t, \\n, \\f, \\r, \\\", \\\', \\\\"
            )

    val esc =
        ((c: Char) => utility.escapedChars.apply(c)) <#> escapedChar

    val character: Parsley[Char] =
        (noneOf('"', '\'', '\\') <|> ("\\" *> esc)).label("character")

    // int-sign := '+' | '-'
    val intSign =
        ("+" #> identity[Long] _ <|> "-" #> ((x: Long) => -x))
            .label("integer sign")

    /* LITERALS */

    // char-liter := ''' <character> '''
    val charLiter: Parsley[ExprNode] =
        CharLiterNode(
          lexer.lexeme("'" ~> character <~ "'")
        )

    // int-liter := <int-sign>? <digit>+
    val intLiter: Parsley[ExprNode] =
        IntLiterNode(
          (intSign <*> number.label("number after integer sign") <|> number)
              .collectMsg("Integer Overflow") {
                  case x if x.toInt == x => x.toInt
              }
              .label("integer literal")
        )

    // TODO: Try to use implicits to not use 'lex.keyword' everytime
    // bool-liter := true | false
    val boolLiter: Parsley[ExprNode] =
        BoolLiterNode(
          (lexer.keyword("true") #> true <|> lexer.keyword("false") #> false)
              .label("boolean literal")
        )

    // pair-liter := null
    val pairLiter: Parsley[ExprNode] =
        lexer.keyword("null").label("pair literal") *> PairLiterNode()

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
              .label("string literal")
        )

    // expression atoms
    val exprAtoms: Parsley[ExprNode] =
        (intLiter <|> boolLiter <|> charLiter <|> stringLiter <|> pairLiter <|>
            ident)
            .label(
              "expression atoms" //(ie. identifier, integer, boolean, character, string or a pair literal)"
            )
            .explain(
              "- Expression Atoms: identifier, integer, boolean, character, string or a pair literal"
            )

    /* TYPES */
    // base-type := 'int' | 'bool' | 'char' | 'string'
    lazy val intType = IntTypeNode <# lexer.keyword("int").label("Int Type")
    lazy val boolType = BoolTypeNode <# lexer.keyword("bool").label("Bool Type")
    lazy val charType = CharTypeNode <# lexer.keyword("char").label("Char Type")
    lazy val stringType =
        StringTypeNode <# lexer.keyword("string").label("String Type")
    lazy val baseType: Parsley[BaseTypeNode] =
        (intType <|> boolType <|> charType <|> stringType)
            .label("Base Types: int, bool, char, string")
    val pairBaseType =
        PairElemTypePairNode <# lexer.keyword("pair").label("Pair Base Type")

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
              "(" ~> sepBy(param, ",").label("function parameters") <~ ")",
              "is" ~> stat
                  .collectMsg(
                    "Missing return or exit statement on a path"
                  )(verifyFuncExit)
                  .collectMsg(
                    "Return Statement not at end of statement block"
                  )(verifyCleanExit) <~ "end"
            ).label("function declaration")
                .explain(
                  "- Function Declaration Syntax: <return-type> <function-name> ( <list-of-parameters> ) is <statement> end"
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
          ("(" *> expr <* ")").label("bracketed expressions")
              <|> attempt(arrayElem)
              <|> exprAtoms
        )(
          Ops(Prefix)(
            Not <# "!".label("unary operator"),
            Neg <# attempt("-" <~ notFollowedBy(number))
                .label("unary operator"),
            Len <# "len".label("unary operator"),
            Ord <# "ord".label("unary operator"),
            Chr <# "chr".label("unary operator")
          ),
          Ops(InfixL)(
            Mult <# "*".label("binary operator"),
            Div <# "/".label("binary operator"),
            Mod <# "%".label("binary operator")
          ),
          Ops(InfixL)(
            Add <# "+".label("binary operator"),
            Sub <# "-".label("binary operator")
          ),
          Ops(InfixL)(
            GTE <# ">=".label("comparison operator"),
            LTE <# "<=".label("comparison operator"),
            LT <# "<".label("comparison operator"),
            GT <# ">".label("comparison operator")
          ),
          Ops(InfixL)(
            Equal <# "==".label("equality operator"),
            NotEqual <# "!=".label("equality operator")
          ),
          Ops(InfixL)(And <# "&&".label("logical operator")),
          Ops(InfixL)(Or <# "||".label("logical operator"))
        )
    // array-elem := identifier ('[' <expr> ']')+
    lazy val arrayElem =
        ArrayElemNode(ident, some("[" *> expr <* "]"))
            .label("array element") // <identifier> [<expression>]")
            .explain(
              "- Array Elements: <identifier> [<expression>]"
            )

    // pair-elem := 'fst' expr <|> 'snd' expr
    lazy val firstPairElem = FirstPairElemNode("fst" *> expr)
    lazy val secondPairElem = SecondPairElemNode("snd" *> expr)

    lazy val pairElem = (firstPairElem <|> secondPairElem)
        .label("pair element: 'fst <Expression>' or 'snd <Expression>'")
    // .explain( "- Structure for Pair Elements: 'fst <Expression>' or 'snd <Expression>'")

    /* ASSIGNMENTS */
    // assignLHS := ident <|> array-elem <|> pair-elem
    lazy val assignLHS = (attempt(arrayElem) <|> ident <|> pairElem)
        .label("LHS Assignment: array element, identifier or pair element")

    // arg-list := expr (',' expr )*
    lazy val exprArgList: Parsley[List[ExprNode]] = sepBy(expr, ",")

    // Variables newPair, arrayLiter and call are for assign-rhs parsing
    // newPair := 'newpair''(' expr ',' expr ')'
    lazy val newPair =
        NewPairNode(
          "newpair".label("pair constructor") *> "(" *> expr <* ",",
          expr <* ")"
        )

    // call := ‘call’ ⟨ident⟩ ‘(’ ⟨arg-list⟩? ‘)’
    lazy val call =
        CallNode(
          "call".label("function call") *> ident.label("function name"),
          "(".label("\"(\" <function arguments> \")\"") *> exprArgList.label(
            "function arguments"
          ) <* ")"
        )

    // array-liter := ‘[’ ( ⟨expr ⟩ (‘,’ ⟨expr ⟩)* )? ‘]’
    // ***Note: difference between option vs. optional?
    lazy val arrayLiter = ArrayLiterNode(
      ("[" *> sepBy(expr, ",") <* "]").label("array literal")
    )

    // assign-rhs := expr <|> array-liter <|> 'newpair' '(' expr ',' expr ')'
    // <|> pairElem <|> 'call' ident '(' arg-list? ')'
    lazy val assignRHS =
        (expr <|> arrayLiter <|> newPair <|> pairElem.hide <|> call)
            .label("RHS Assignment")
            .explain(
              "Valid RHS Assignments include: Expression, Array Literal, New Pair, Pair Element, Function Call"
            )

    /* STATEMENTS */
    lazy val skipStat = SkipNode <# "skip".label("skip statement")
    lazy val newAssignStat =
        NewAssignNode(
          anyType,
          ident,
          "=".label("\"= <RHS assignment>\"") *> assignRHS
        )
            .label(
              "new assignment statement: <anyType> <identifier> '=' <assign-rhs> "
            )
    //.explain("- New Assignment Statement: <anyType> <identifier> '=' <assign-rhs> ")
    lazy val lrAssignStat =
        LRAssignNode(assignLHS, "=" *> assignRHS)
            .label("assignment statement: <assign-lhs> '=' <assign-rhs>")
    //.explain("- Assignment Statement: <assign-lhs> '=' <assign-rhs>")
    lazy val readStat =
        ReadNode("read" *> assignLHS).label("read statement: read <Expression>")
    lazy val freeStat =
        FreeNode("free" *> expr).label("free statement: free <Expression>")
    lazy val returnStat = ReturnNode("return" *> expr).label(
      "return statement: return <Expression>"
    )
    lazy val exitStat =
        ExitNode("exit" *> expr).label("exit statement: exit <Expression>")
    lazy val printStat =
        PrintNode("print" *> expr).label("print statement: print <Expression>")
    lazy val printlnStat = PrintlnNode("println" *> expr).label(
      "println statement: println <Expression>"
    )

    // if-else-stat := ‘if’ ⟨expr ⟩ ‘then’ ⟨stat ⟩ ‘else’ ⟨stat ⟩ ‘fi’
    lazy val ifThenElseStat: Parsley[StatNode] =
        IfThenElseNode(
          "if" *> expr,
          "then" *> stat,
          "else" *> stat <* "fi"
        ).label("if-else conditional")

    // while-do-stat := ‘while’ ⟨expr ⟩ ‘do’ ⟨stat ⟩ ‘done’
    lazy val whileDoStat =
        WhileDoNode(
          "while".label("while statement") *> expr,
          "do".label("do-while loop body: do <Statement>") *> stat <* "done"
              .label("do-while loop closing \"done\" keyword")
        )

    // begin-end-stat := ‘begin’ ⟨stat ⟩ ‘end’
    lazy val beginEndStat =
        BeginEndNode(
          "begin".label("begin-end statement") *> stat <* "end".label(
            "begin-end closing \"end\""
          )
        )

    lazy val statList: Parsley[StatNode] =
        StatListNode(
          sepBy1(statAtoms.label("next statement atom after \";\""), ";")
        )
    // stat := 'skip' | ⟨type ⟩ ⟨ident ⟩ ‘=’ ⟨assign-rhs ⟩
    // 	| ⟨assign-lhs ⟩ ‘=’ ⟨assign-rhs ⟩ | ‘read’ ⟨assign-lhs ⟩
    //  | ‘free’ ⟨expr ⟩ | ‘return’ ⟨expr ⟩ | ‘exit’ ⟨expr ⟩
    //  | ‘print’ ⟨expr ⟩ | ‘println’ ⟨expr ⟩
    //  | ‘if’ ⟨expr ⟩ ‘then’ ⟨stat ⟩ ‘else’ ⟨stat ⟩ ‘fi’
    //  | ‘while’ ⟨expr ⟩ ‘do’ ⟨stat ⟩ ‘done’
    //  | ‘begin’ ⟨stat ⟩ ‘end’ | ⟨stat ⟩ ‘;’ ⟨stat ⟩
    lazy val statAtoms: Parsley[StatNode] =
        (skipStat <|> newAssignStat <|> lrAssignStat <|> readStat <|>
            freeStat <|> returnStat <|> exitStat <|> printStat <|>
            printlnStat <|> ifThenElseStat <|> whileDoStat <|> beginEndStat)
            .label("statement atoms")
            .explain(
              "- Statement atoms include: 'skip', Assignment, Read, Free, Return, Exit, Print, Conditional or Begin-end statements"
            )

    lazy val stat: Parsley[StatNode] =
        (statList <|> statAtoms <* notFollowedBy(";")).label("statement")

    /* TYPES */
    // type := <base-type> | <array-type> | <pair-type>
    lazy val anyType: Parsley[TypeNode] =
        attempt(arrayType) <|> pairType <|> baseType

    // array-type := <type> '[' ']'
    lazy val arrayType: Parsley[ArrayTypeNode] =
        chain
            .postfix1(
              ArrayTypeNode(pairType <|> baseType, 0),
              ArrayTypeNode("[]").label("brackets for Array Type \"[]\"")
            )
            .label("Array Type")

    // pair-elem-type := <base-type> | <array-type> | 'pair'
    lazy val pairElemType: Parsley[PairElemTypeNode] =
        (attempt(arrayType) <|> baseType <|> pairBaseType)
            .label("Pair Element Type: any valid type")

    // pair-type := 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
    lazy val pairType: Parsley[PairTypeNode] = PairTypeNode(
      "pair" *> "(".label(
        " \"(\" <Any Type> , <Any Type> \")\" "
      ) *> pairElemType <* ",",
      pairElemType <* ")"
    ).label("Pair Type")
}
