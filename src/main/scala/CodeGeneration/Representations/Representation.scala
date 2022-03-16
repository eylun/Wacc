trait Representation {
    def generateOperand(op: SecondOperand): String

    def generateShiftValue(s: Shift): String

    def generateLine(i: Instruction)(implicit collector: WaccBuffer, repr: Representation): String

    def generateAdd(i: Instruction): String

    def generateCompare(i: Instruction)(implicit collector: WaccBuffer): String

    def generateBranch(i: Instruction): String

    def generateLoad(i: Instruction): String

    def generateLogicalBinOp(i: Instruction)(implicit collector: WaccBuffer): String

    def generateMove(i: Instruction): String

    def generateMultiply(i: Instruction): String

    def generatePush(i: Instruction): String

    def generatePop(i: Instruction): String

    def generateStore(i: Instruction): String

    def generateSub(i: Instruction): String
}
