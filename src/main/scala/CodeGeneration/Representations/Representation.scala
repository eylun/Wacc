trait Representation {
    def generateLine(i: Instruction)(implicit collector: WaccBuffer, repr: Representation): String

    def generateAdd(i: Instruction): String

    def generateCompare(i: Instruction)(implicit collector: WaccBuffer): String

    def generateBranch(i: Instruction): String

    def generateLogicalBinOp(i: Instruction)(implicit collector: WaccBuffer): String

    def generateMove(i: Instruction): String

    def generatePush(i: Instruction): String

    def generatePop(i: Instruction): String

    def generateSub(i: Instruction): String
}
