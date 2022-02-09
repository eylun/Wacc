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
        "- Escape Sequences include: \\0, \\b, \\t, \\n, \\f, \\r, \\\", \\\', \\\\"
      )

  val esc =
    ((c: Char) => utility.escapedChars.apply(c)) <#> escapedChar

  val character: Parsley[Char] =
    (noneOf('"', '\'', '\\') <|> ("\\" *> esc)).label("Character")

  // int-sign := '+' | '-'
  val intSign =
    ("+" #> identity[Long] _ <|> "-" #> ((x: Long) => -x)).label("Integer Sign")

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
        .label("Integer Literal")
    )

  // TODO: Try to use implicits to not use 'lex.keyword' everytime
  // bool-liter := true | false
  val boolLiter: Parsley[ExprNode] =
    BoolLiterNode(
      (lexer.keyword("true") #> true <|> lexer.keyword("false") #> false)
        .label("Boolean Literal")
    )

  // pair-liter := null
  val pairLiter: Parsley[ExprNode] =
    lexer.keyword("null").label("Pair Literal") *> PairLiterNode()

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
        .label("String Literal")
    )

  // expression atoms
  val exprAtoms: Parsley[ExprNode] =
    (intLiter <|> boolLiter <|> charLiter <|> stringLiter <|> pairLiter <|>
      ident)
      .label("expression atom (ie. identifier, integer, boolean, character, string or a pair literal)")
      // .explain("- Expression atom includes: identifier, integer, boolean, character, string or a pair literal")

  /* TYPES */
  // base-type := 'int' | 'bool' | 'char' | 'string'
  lazy val intType = IntTypeNode <# lexer.keyword("int").label("Int Type")
  lazy val boolType = BoolTypeNode <# lexer.keyword("bool").label("Bool Type")
  lazy val charType = CharTypeNode <# lexer.keyword("char").label("Char Type")
  lazy val stringType =
    StringTypeNode <# lexer.keyword("string").label("String Type")
  lazy val baseType: Parsley[BaseTypeNode] =
    (intType <|> boolType <|> charType <|> stringType)
      .label("Base Type: int, bool, char, string")
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
      ).label("Function Declaration")
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
          case (ReturnNode(_) | ExitNode(_), n) if n == s.length - 1 =>
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
      ("(" *> expr <* ")").label("bracketed expression: ( <Expression> )")
        <|> attempt(arrayElem)
        <|> exprAtoms
    )(
      Ops(Prefix)(
        Not <# "!".label("unary Operator"),
        Neg <# attempt("-" <~ notFollowedBy(number)).label("Unary Operator"),
        Len <# "len".label("Unary Operator"),
        Ord <# "ord".label("Unary Operator"),
        Chr <# "chr".label("Unary Operator")
      ),
      Ops(InfixL)(Mult <# "*".label("Binary Operator"), Div <# "/".label("Binary Operator"), Mod <# "%".label("Binary Operator")),
      Ops(InfixL)(Add <# "+".label("Binary Operator"), Sub <# "-".label("Binary Operator")),
      Ops(InfixL)(GTE <# ">=".label("Comparison Operator"), LTE <# "<=".label("Comparison Operator"), LT <# "<".label("Comparison Operator"), GT <# ">".label("Comparison Operator")),
      Ops(InfixL)(Equal <# "==".label("Equality Operator"), NotEqual <# "!=".label("Equality Operator")),
      Ops(InfixL)(And <# "&&".label("Logical Operator")),
      Ops(InfixL)(Or <# "||".label("Logical Operator"))
    )
  // array-elem := identifier ('[' <expr> ']')+
  lazy val arrayElem =
    ArrayElemNode(ident, some("[" *> expr <* "]"))
      .label("array element: <identifier> [<expression>]")
      // .explain("- Structure for Array Elements: <identifier> [<expression>]")

  // pair-elem := 'fst' expr <|> 'snd' expr
  lazy val firstPairElem = FirstPairElemNode("fst" *> expr)
  lazy val secondPairElem = SecondPairElemNode("snd" *> expr)

  lazy val pairElem = (firstPairElem <|> secondPairElem)
    .label("Pair Element: 'fst <Expression>' or 'snd <Expression>'")
    // .explain( "- Structure for Pair Elements: 'fst <Expression>' or 'snd <Expression>'")

  /* ASSIGNMENTS */
  // assignLHS := ident <|> array-elem <|> pair-elem
  lazy val assignLHS = (attempt(arrayElem) <|> ident <|> pairElem).label("LHS Assignment: array element, identifier or pair element")

  // arg-list := expr (',' expr )*
  lazy val exprArgList: Parsley[List[ExprNode]] = sepBy(expr, ",")

  // Variables newPair, arrayLiter and call are for assign-rhs parsing
  // newPair := 'newpair''(' expr ',' expr ')'
  lazy val newPair =
    NewPairNode(
      "newpair".label("Pair Constructor") *> "(" *> expr <* ",",
      expr <* ")"
    )

  // call := ‘call’ ⟨ident⟩ ‘(’ ⟨arg-list⟩? ‘)’
  lazy val call =
    CallNode("call".label("Function Call") *> ident.label("function name"),
     "(".label("\"(\" <function arguments> \")\"") *> exprArgList.label("function arguments") <* ")")

  // array-liter := ‘[’ ( ⟨expr ⟩ (‘,’ ⟨expr ⟩)* )? ‘]’
  // ***Note: difference between option vs. optional?
  lazy val arrayLiter = ArrayLiterNode(
    ("[" *> sepBy(expr, ",") <* "]").label("Array Literal")
  )

  // assign-rhs := expr <|> array-liter <|> 'newpair' '(' expr ',' expr ')'
  // <|> pairElem <|> 'call' ident '(' arg-list? ')'
  lazy val assignRHS = (expr <|> arrayLiter <|> newPair <|> pairElem.hide <|> call)
  .label("RHS Assignment")
  .explain("Valid RHS Assignments include: Expression, Array Literal, New Pair, Pair Element, Function Call")

  /* STATEMENTS */
  lazy val skipStat = SkipNode <# "skip".label("Skip Statement")

  lazy val newAssignStat =
    NewAssignNode(anyType, ident, "=".label("\"= <RHS assignment>\"") *> assignRHS)
      .label("New Assignment Statement: <anyType> <identifier> '=' <assign-rhs> ")
      //.explain("- New Assignment Statement: <anyType> <identifier> '=' <assign-rhs> ")

  lazy val lrAssignStat = 
    LRAssignNode(assignLHS, "=" *> assignRHS)
      .label("Assignment Statement: <assign-lhs> '=' <assign-rhs>")
      //.explain("- Assignment Statement: <assign-lhs> '=' <assign-rhs>")
  lazy val readStat = ReadNode("read" *> assignLHS).label("Read Statement: read <Expression>")
  lazy val freeStat = FreeNode("free" *> expr).label("Free Statement: free <Expression>")
  lazy val returnStat = ReturnNode("return" *> expr).label("Return Statement: return <Expression>")
  lazy val exitStat = ExitNode("exit" *> expr).label("Exit Statement: exit <Expression>")
  lazy val printStat = PrintNode("print" *> expr).label("Print Statement: print <Expression>")
  lazy val printlnStat = PrintlnNode("println" *> expr).label("Println Statement: println <Expression>")

  // if-else-stat := ‘if’ ⟨expr ⟩ ‘then’ ⟨stat ⟩ ‘else’ ⟨stat ⟩ ‘fi’
  lazy val ifThenElseStat: Parsley[StatNode] =
    IfThenElseNode(
      "if".label("statement if") *> expr,
      "then".label("if statement then") *> stat,
      "else".label("if statement else") *> stat <* "fi".label(
          "if statement fi"
        )
    ).label("If-Else Conditional")

  // while-do-stat := ‘while’ ⟨expr ⟩ ‘do’ ⟨stat ⟩ ‘done’
  lazy val whileDoStat =
    WhileDoNode("while".label("While Statement") *> expr, "do".label("do-while loop body: do <Statement>") *> stat <* "done".label("do-while loop closing \"done\" keyword"))

  // begin-end-stat := ‘begin’ ⟨stat ⟩ ‘end’
  lazy val beginEndStat =
    BeginEndNode("begin".label("begin-end statement") *> stat <* "end".label("begin-end closing \"end\""))

  lazy val statList: Parsley[StatNode] =
    StatListNode(sepBy1(statAtoms.label("next statement atom after \";\""), ";"))
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
      .label("Statement Atoms")
      .explain(
        "- Statement atoms include: 'skip', Assignment, Read, Free, Return, Exit, Print, Conditional or Begin-end statements"
      )

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
      ArrayTypeNode("[]").label("brackets for Array Type \"[]\"")
    )

  // pair-elem-type := <base-type> | <array-type> | 'pair'
  lazy val pairElemType: Parsley[PairElemTypeNode] =
    attempt(arrayType) <|> baseType <|> pairBaseType

  // pair-type := 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
  lazy val pairType: Parsley[PairTypeNode] = PairTypeNode(
    "pair" *> "(" *> pairElemType <* ",",
    pairElemType <* ")"
  ).label("Pair Type")
}
