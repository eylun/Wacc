import parsley.Parsley
import parsley.Parsley._

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
    "*" | "/" | "%" | "+" | "-" | ">" | ">=" | "<" | "<=" | "==" | "!=" | "&&" | "||"
  )
}

/* Lexer */
object lexer {
  import parsley.character._
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

  def fully[A](p: => Parsley[A]): Parsley[A] = lex.whiteSpace ~> p <~ eof

  val number = lexer.decimal
  val identifier = lexer.identifier

  // escaped-char := '0' | 'b' | 't' | 'n' | 'f' | 'r' | '"' | ''' | '\'
  val escapedChar =
    "0" <|> "b" <|> "t" <|> "n" <|> "f" <|> "r" <|> "\"" <|> "'" <|> "\\"

  val charLit = Parser(
    "'" ~> (noneOf('"', '\'', '\\') <|> ("\\" <~> escapedChar)) <~ "'"
  )

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
  import lexer.{fully, lex}
  import parsley.implicits.character.{charLift, stringLift}
  import parsley.character.{anyChar, digit, endOfLine, noneOf}
  import parsley.combinator.{eof, many, manyUntil, optional, some}

  /* COMMENTS */

  // comment := '#' (any character except EOL)* <EOL>
  // val comment = "#" ~> manyUntil(anyChar, endOfLine)

  /* LITERALS */

  // int-sign := '+' | '-'
  val intSign = "+" <|> "-"

  // int-liter := <int-sign>? <digit>+
  val intLiter = optional(intSign) <~> some(digit)

  // TODO: Try to use implicits to not use 'lex.keyword' everytime
  // bool-liter := true | false
  val boolLiter = lex.keyword("true") <|> lex.keyword("false")

  // pair-liter := null
  val pairLiter = "null"

  // string-liter := '"' <character>* '"'
  val stringLiter = "\"" ~> many(character) <~ "\""

  // ident := ('_' | 'a'-'z' | 'A'-'Z') ('_' | 'a'-'z' | 'A'-'Z' | '0'-'9')*
  // val ident = ???

  // TODO: remove - this is here for testing purposes
  val literal =
    intLiter <|> boolLiter <|> pairLiter <|> charLiter <|> stringLiter <|> lex
      .keyword("comment")

  // TODO: change this at the end - currently set to check literals
  // program := 'begin' <func>* <stat> 'end'
  val program = fully(lex.keyword("begin") ~> literal)
}
