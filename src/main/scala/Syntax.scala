import parsley.Parsley
import parsley.Parsley._
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

  def fully[A](p: => Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof

  val digit = lexer.decimal
  val identifier = lexer.identifier

  /* LITERALS */

  // escaped-char := '0' | 'b' | 't' | 'n' | 'f' | 'r' | '"' | ''' | '\'
  val escapedChar =
    "0" <|> "b" <|> "t" <|> "n" <|> "f" <|> "r" <|> "\"" <|> "'" <|> "\\"

  val character = noneOf('"', '\'', '\\') <|> ("\\" <~> escapedChar)

  // int-sign := '+' | '-'
  val intSign = lexer.maxOp("+") <|> lexer.maxOp("-")

  // char-liter := ''' <character> '''
  val charLiter =
    lexer.whiteSpace ~> "'" ~> character <~ "'" <~ lexer.whiteSpace

  // int-liter := <int-sign>? <digit>+
  val intLiter = optional(intSign) <~> some(digit)

  // TODO: Try to use implicits to not use 'lex.keyword' everytime
  // bool-liter := true | false
  val boolLiter = lexer.keyword("true") <|> lexer.keyword("false")

  // pair-liter := null
  val pairLiter = lexer.keyword("null")

  // string-liter := '"' <character>* '"'
  val stringLiter =
    lexer.whiteSpace ~> "\"" ~> many(character) <~ "\"" <~ lexer.whiteSpace

  // ident := ('_' | 'a'-'z' | 'A'-'Z') ('_' | 'a'-'z' | 'A'-'Z' | '0'-'9')*
  val ident = ("_" <|> letter) *> many("_" <|> letter <|> digit)

  // TODO: remove - this is here for testing purposes
  val literal =
    intLiter <|> boolLiter <|> pairLiter <|> charLiter <|> stringLiter
  
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
  import lexer.{fully, literal}
  import lexer.implicits.implicitLexeme
  import parsley.combinator.{eof, many, manyUntil, optional, some}

  // TODO: change this at the end - currently set to check literals
  // program := 'begin' <func>* <stat> 'end'
  lazy val program = "begin" ~> many(literal) <~ "end"

  val parse = fully(program)

  // expr := literal <|> identifier <|> array-elem <|> unary op  <|> bin op <|> paren
  // array-elem := identifier ('[' <expr> ']')+
  // unary-oper
  lazy val expr: Parsley[literal] = precedence[literal](literal, identifier, '(' *> expr <* ')')(
    Ops(Prefix)(unaryOp #> ()
  )

}
