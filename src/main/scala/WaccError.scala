import parsley.errors.ErrorBuilder
case class WaccError(
    pos: (Int, Int),
    source: Option[String],
    lines: WaccErrorLines
)

sealed trait WaccErrorLines
case class VanillaError(
    unexpected: Option[WaccErrorItem],
    expecteds: Set[WaccErrorItem],
    reasons: Set[String]
) extends WaccErrorLines
case class SpecialisedError(msgs: Set[String]) extends WaccErrorLines

sealed trait WaccErrorItem
case class WaccRaw(item: String) extends WaccErrorItem
case class WaccNamed(item: String) extends WaccErrorItem
case object WaccEndOfInput extends WaccErrorItem

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

    override def raw(item: String): Raw = WaccRaw(item)

    override def named(item: String): Named = WaccNamed(item)

    override val endOfInput: EndOfInput = WaccEndOfInput

}
