import java.io.{File, BufferedWriter, FileWriter}

object ARMRepresentation extends Representation {
    implicit val repr: Representation = this

    def apply(
        progNode: ProgramNode,
        st: SymbolTable,
        filename: String
    ): Unit = {
        implicit val collector: WaccBuffer = new WaccBuffer
        collector.setupMain()
        val bw = new BufferedWriter(new FileWriter(new File(filename)))
        CodeGenerator(progNode, st).foreach(l => bw.write(generateLine(l)))
        bw.close()
    }

    /** generateLine() matches an instruction to its relevant assembly code
      * representation
      */
    def generateLine(instr: Instruction)(implicit collector: WaccBuffer, repr: Representation): String =
        s"${instr match {
            case Label(labelName) => s"$labelName:"
            case Directive(name)  => s".$name"
            case PushInstr(reg)   => generatePush(instr)
            case PopInstr(_)      => generatePop(instr)
            case MoveInstr(dst, src, cond)        => generateMove(instr)

            /** Logical Instructions */
            case AndInstr(_, _, _, _, _) | OrInstr(_, _, _, _, _) | XorInstr(_, _, _, _, _) 
                => generateLogicalBinOp(instr)

            /** Arithmetic Instructions */
            case AddInstr(_, _, _, _) => generateAdd(instr)
            case SubInstr(_, _, _, _) | ReverseSubInstr(_, _, _, _) => generateSub(instr)
            case SMullInstr(rdLo, rdHi, fst, snd, true) =>
                s"\tSMULLS $rdLo, $rdHi, $fst, $snd"
            case SMullInstr(rdLo, rdHi, fst, snd, false) =>
                s"\tSMULL $rdLo, $rdHi, $fst, $snd"


            /** Load Instructions */
            case LoadLabelInstr(_, _, _) | LoadImmIntInstr(_, _, _) | LoadInstr(_, _, _, _) | 
                 LoadRegSignedByte(_, _, _, _) => generateLoad(instr)

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
            case BranchInstr(_, _) | BranchLinkInstr(_, _) => generateBranch(instr)

            /** Comparison Instructions */
            case CompareInstr(_, _, _) => generateCompare(instr)

            /** For unmatched cases, "temp" will be generated. Used for
              * debugging.
              */
            case _ => "Temp"
        }}\n"

    def generatePush(i: Instruction): String = {
        i match {
            case PushInstr(regs) => s"\tPUSH {${regs.mkString(", ")}}"
            case _ => ""
        }
    }

    def generatePop(i: Instruction): String = {
        i match {
            case PopInstr(regs) => s"\tPOP {${regs.mkString(", ")}}"
            case _ => ""
        }
    }

    def generateLogicalBinOp(i: Instruction)(implicit collector: WaccBuffer): String = {
        i match {
            case AndInstr(dst, fst, snd, false, cond) => s"\tAND$cond $dst, $fst, $snd"
            case AndInstr(dst, fst, snd, true, cond) => s"\tANDS$cond $dst, $fst, $snd"
            case XorInstr(dst, fst, snd, false, cond) => s"\tEOR$cond $dst, $fst, $snd"
            case XorInstr(dst, fst, snd, true, cond) => s"\tEOR${cond}S $dst, $fst, $snd"
            case OrInstr(dst, fst, snd, false, cond) => s"\tORR$cond $dst, $fst, $snd"
            case OrInstr(dst, fst, snd, true, cond) => s"\tORR${cond}S $dst, $fst, $snd"
            case _ => ""
        }
    }

    def generateAdd(i: Instruction): String = {
        i match {
            case AddInstr(dst, fst, snd, true)  => s"\tADDS $dst, $fst, $snd"
            case AddInstr(dst, fst, snd, false) => s"\tADD $dst, $fst, $snd"
            case _ => ""
        }
    }

    def generateSub(i: Instruction): String = {
        i match {
            case SubInstr(dst, fst, snd, true) => s"\tSUBS $dst, $fst, $snd"
            case SubInstr(dst, fst, snd, false) => s"\tSUB $dst, $fst, $snd"
            case ReverseSubInstr(dst, fst, snd, true) => s"\tRSBS $dst, $fst, $snd"
            case ReverseSubInstr(dst, fst, snd, false) => s"\tRSB $dst, $fst, $snd"
            case _ => ""
        }
    }

    def generateMove(i: Instruction): String = {
        i match {
            case MoveInstr(dst, src, cond) => s"\tMOV$cond $dst, $src"
            case _ => ""
        }
    }

    def generateLoad(i: Instruction): String = {
        i match {
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
            case _ => ""
        }
    }

    def generateBranch(i: Instruction): String = {
        i match {
            case BranchInstr(label, cond) => s"\tB$cond $label"
            case BranchLinkInstr(label, cond) => s"\tBL$cond $label"
            case _ => ""
        }
    }

    def generateCompare(i: Instruction)(implicit collector: WaccBuffer): String = {
        i match {
            case CompareInstr(fst, snd, cond) => s"\tCMP$cond $fst, $snd"
            case _ => ""
        }
    }
}
