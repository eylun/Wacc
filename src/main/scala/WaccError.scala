import parsley.errors.ErrorBuilder

case class WaccError(
    pos: (Int, Int),
    source: Option[String],
    lines: WaccErrorLines,
    syntaxError: Boolean = true
){
    def render() = {
        var errorType = "Syntax Error: "
        if (!syntaxError) {
            errorType = "Semantic Error: "
        }
        val src = "In file \'" + source.get + "\' "
        val position = "(line " + pos._1 + " , column " + pos._2 + ")"
        var errorMsg = "Error message here"

        if(lines.isInstanceOf[VanillaError]){
            val vanillaError = lines.asInstanceOf[VanillaError]
            val unexpected = "unexpected: \"" + vanillaError.getUnexpecteds() + "\""
            val expected = "expected: " + vanillaError.getExpecteds()
            
            if(vanillaError.getReasons().size == 0){
                errorMsg = errorType + src + position + "\n" + unexpected + "\n" + expected
            } else {
                val explanation = "explanation: " + vanillaError.getReasons()
                errorMsg = errorType + src + position + "\n" + unexpected + "\n" + expected + "\n" + explanation
            }
            
        }

        if(lines.isInstanceOf[SpecialisedError]){
            val specializedError = lines.asInstanceOf[SpecialisedError]
            errorMsg = errorType + src + position + "\n" + specializedError.getMsgs()
        }

        println(errorMsg)
    }
}

sealed trait WaccErrorLines

case class VanillaError(
    unexpected: Option[WaccErrorItem],
    expecteds: Set[WaccErrorItem],
    reasons: Set[String]
) extends WaccErrorLines {

    def getUnexpecteds() = {
        if(unexpected.isEmpty){
            ""
        } else {
            unexpected.get.getItem()
        }
    }

    def getExpecteds() = {
        var result = ""
        for(expected <- expecteds){
            if(result.size == 0){
                result = expected.getItem()
            } else {
                result = result + " " + expected.getItem()
            }
        }
        result
    }

    def getReasons() = {
        var allReasons = ""
        for(reason <- reasons){
            if(allReasons.size == 0){
                allReasons = reason
            } else {
                allReasons = allReasons + "\n" + reason
            }
        }
        allReasons
    }
}

case class SpecialisedError(msgs: Set[String]) extends WaccErrorLines {
    def getMsgs() = {
        var msgs = ""
        for(msg <- msgs){
            msgs = msgs + msg + "\n"
        }
        msgs
    }
}

sealed trait WaccErrorItem {
    def getItem(): String
}
case class WaccRaw(item: String) extends WaccErrorItem {
    def getItem () = { item }
}
case class WaccNamed(item: String) extends WaccErrorItem {
    def getItem () = { item }
}
case object WaccEndOfInput extends WaccErrorItem {
    def getItem () = { "End of Input" }
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

    override def raw(item: String): Raw = WaccRaw(item)

    override def named(item: String): Named = WaccNamed(item)

    override val endOfInput: EndOfInput = WaccEndOfInput

}
