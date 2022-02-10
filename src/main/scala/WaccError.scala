import parsley.errors.ErrorBuilder
import parsley.Parsley, Parsley._

case class WaccError(
    pos: (Int, Int),
    source: Option[String] = None,
    lines: WaccErrorLines,
    syntaxError: Boolean = true
) {
    def render() = {
        val sb = syntaxError match {
            case true  => new StringBuilder("Syntax Error Detected at ")
            case false => new StringBuilder("Semantic Error Detected at ")
        }
        sb ++= s"(line ${pos._1}, column ${pos._2})\n"

        lines match {
            case vanillaError @ VanillaError(
                  unexpected,
                  expecteds,
                  reasons
                ) => {
                sb ++= (unexpected match {
                    case None => ""
                    case Some(item) =>
                        s"unexpected input: ${item.getItem()}"
                })
                sb += '\n'
                sb ++= s"expected input: ${expecteds
                    .map {
                        case WaccRaw(s)   => s"\'${s}\'"
                        case WaccNamed(s) => s
                        case _            => "End of Input"
                    }
                    .mkString(", ")}"
                if (reasons.size > 0) {
                    sb += '\n'
                    sb ++= reasons mkString "\n"
                }
            }
            case specializedError @ SpecialisedError(messages) => {
                sb ++= messages mkString "\n"
            }
        }
        println(sb)
    }
}

object WaccError {
    def apply(pos: (Int, Int), errorLine: String): WaccError =
        WaccError(pos, None, SpecialisedError(Set(errorLine)), false)

}

sealed trait WaccErrorLines

case class VanillaError(
    unexpected: Option[WaccErrorItem],
    expecteds: Set[WaccErrorItem],
    reasons: Set[String]
) extends WaccErrorLines

case class SpecialisedError(msgs: Set[String]) extends WaccErrorLines

sealed trait WaccErrorItem {
    def getItem(): String
}
case class WaccRaw(item: String) extends WaccErrorItem {
    def getItem() = s"\'${item}\'"
}
case class WaccNamed(item: String) extends WaccErrorItem {
    def getItem() = item
}
case object WaccEndOfInput extends WaccErrorItem {
    def getItem() = "End of Input"
}

class WaccErrorBuilder extends ErrorBuilder[WaccError] {
    type Position = (Int, Int)
    type Source = Option[String]
    type ErrorInfoLines = WaccErrorLines

    type UnexpectedLine = Option[WaccErrorItem]
    type ExpectedLine = Set[WaccErrorItem]
    type Messages = Set[String]
    type Message = String

    type ExpectedItems = Set[WaccErrorItem]

    type Item = WaccErrorItem
    type Raw = WaccRaw
    type Named = WaccNamed
    type EndOfInput = WaccEndOfInput.type

    override def format(
        pos: Position,
        source: Source,
        lines: ErrorInfoLines
    ): WaccError = WaccError(pos, source, lines)

    override def pos(line: Int, col: Int): Position = (line, col)

    override def source(sourceName: Option[String]): Source = sourceName

    override def vanillaError(
        unexpected: UnexpectedLine,
        expected: ExpectedLine,
        reasons: Messages,
        line: LineInfo
    ): ErrorInfoLines = VanillaError(unexpected, expected, reasons)

    override def specialisedError(
        msgs: Messages,
        line: LineInfo
    ): ErrorInfoLines = SpecialisedError(msgs)

    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

    override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

    override def unexpected(item: Option[Item]): UnexpectedLine = item

    override def expected(alts: ExpectedItems): ExpectedLine = alts

    override def reason(reason: String): Message = reason

    override def message(msg: String): Message = msg

    type LineInfo = Unit
    override def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        errorPointsAt: Int
    ): LineInfo = ()

    override val numLinesBefore: Int = 0

    override val numLinesAfter: Int = 0

    override def raw(item: String): Raw = WaccRaw(
      item.takeWhile(c => c != '\n' && c != ' ')
    )

    override def named(item: String): Named = WaccNamed(item)

    override val endOfInput: EndOfInput = WaccEndOfInput

}
