import java.io.{File, BufferedWriter, FileWriter}
import OptimisationFlag._

object ARMRepresentation extends Representation {

    def apply(
        progNode: ProgramNode,
        st: SymbolTable,
        filename: String,
        optFlag: OptimisationFlag = OptimisationFlag.O0
    ): Unit = {
        implicit val collector: WaccBuffer = new WaccBuffer(optFlag)
        collector.setupMain()
        val bw = new BufferedWriter(new FileWriter(new File(filename)))
        CodeGenerator(progNode, st).foreach(l => bw.write(generateLine(l)))
        bw.close()
    }

    /** generateLine() matches an instruction to its relevant assembly code representation
      */
    def generateLine(instr: Instruction): String =
        s"${instr match {
            case Label(labelName) => s"$labelName:"
            case Directive(name)  => s".$name"
            case PushInstr(reg)   => s"\tPUSH {${reg.mkString(", ")}}"
            case PopInstr(reg)    => s"\tPOP {${reg.mkString(", ")}}"

            /** Logical Instructions */
            case AndInstr(dst, fst, snd, false, cond) =>
                s"\tAND$cond $dst, $fst, $snd"
            case AndInstr(dst, fst, snd, true, cond) =>
                s"\tAND${cond}S $dst, $fst, $snd"
            case XorInstr(dst, fst, snd, false, cond) =>
                s"\tEOR$cond $dst, $fst, $snd"
            case XorInstr(dst, fst, snd, true, cond) =>
                s"\tEOR${cond}S $dst, $fst, $snd"
            case OrInstr(dst, fst, snd, false, cond) =>
                s"\tORR$cond $dst, $fst, $snd"
            case OrInstr(dst, fst, snd, true, cond) =>
                s"\tORR${cond}S $dst, $fst, $snd"

            /** Arithmetic Instructions */
            case AddInstr(dst, fst, snd, true)  => s"\tADDS $dst, $fst, $snd"
            case AddInstr(dst, fst, snd, false) => s"\tADD $dst, $fst, $snd"
            case SubInstr(dst, fst, snd, true)  => s"\tSUBS $dst, $fst, $snd"
            case SubInstr(dst, fst, snd, false) => s"\tSUB $dst, $fst, $snd"
            case ReverseSubInstr(dst, fst, snd, true) =>
                s"\tRSBS $dst, $fst, $snd"
            case ReverseSubInstr(dst, fst, snd, false) =>
                s"\tRSB $dst, $fst, $snd"
            case SMullInstr(rdLo, rdHi, fst, snd, true) =>
                s"\tSMULLS $rdLo, $rdHi, $fst, $snd"
            case SMullInstr(rdLo, rdHi, fst, snd, false) =>
                s"\tSMULL $rdLo, $rdHi, $fst, $snd"

            case MoveInstr(dst, src, cond) => s"\tMOV$cond $dst, $src"

            /** Load Instructions */
            case LoadLabelInstr(dst, label, cond) => s"\tLDR$cond $dst, =$label"
            case LoadImmIntInstr(dst, imm, cond)  => s"\tLDR$cond $dst, =$imm"
            case LoadInstr(dst, src, ImmOffset(0), cond) =>
                s"\tLDR$cond $dst, [$src]"
            case LoadInstr(dst, src, ImmOffset(offset), cond) =>
                s"\tLDR$cond $dst, [$src, #$offset]"
            case LoadRegSignedByte(dst, src, ImmOffset(0), cond) =>
                s"\tLDRSB$cond $dst, [$src]"
            case LoadRegSignedByte(dst, src, ImmOffset(offset), cond) =>
                s"\tLDRSB$cond $dst, [$src, #$offset]"
            case LoadRegSignedByte(dst, src, RegOp(r), cond) => 
                s"\tLDRSB$cond $dst, [$src, $r]"
            /** Store Instructions */
            case StoreInstr(src, dst, ImmOffset(0), true) =>
                s"\tSTR $src, [$dst]!"
            case StoreInstr(src, dst, ImmOffset(offset), true) =>
                s"\tSTR $src, [$dst, #$offset]!"
            case StoreInstr(src, dst, ImmOffset(0), false) =>
                s"\tSTR $src, [$dst]"
            case StoreInstr(src, dst, ImmOffset(offset), false) =>
                s"\tSTR $src, [$dst, #$offset]"

            /** Store Byte Instructions */
            case StoreByteInstr(src, dst, ImmOffset(0), true) =>
                s"\tSTRB $src, [$dst]!"
            case StoreByteInstr(src, dst, ImmOffset(offset), true) =>
                s"\tSTRB $src, [$dst, #$offset]!"
            case StoreByteInstr(src, dst, ImmOffset(0), false) =>
                s"\tSTRB $src, [$dst]"
            case StoreByteInstr(src, dst, ImmOffset(offset), false) =>
                s"\tSTRB $src, [$dst, #$offset]"

            /** Branch Instructions */
            case BranchInstr(label, condition)     => s"\tB$condition $label"
            case BranchLinkInstr(label, condition) => s"\tBL$condition $label"

            /** Comparison Instructions */
            case CompareInstr(fstOp, sndOp, condition) =>
                s"\tCMP$condition $fstOp, $sndOp"

            /** For unmatched cases, "temp" will be generated. Used for debugging.
              */
            case _ => s"Missing: $instr"
        }}\n"
}
