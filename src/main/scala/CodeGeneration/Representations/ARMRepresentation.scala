import java.io.{File, BufferedWriter, FileWriter}
object ARMRepresentation extends Representation {

    implicit val collector: WaccBuffer = new WaccBuffer

    def apply(
        progNode: ProgramNode,
        st: SymbolTable,
        filename: String
    ): Unit = {
        collector.setupMain()
        val bw = new BufferedWriter(new FileWriter(new File(filename)))
        CodeGenerator(progNode, st).foreach(l => bw.write(generateLine(l)))
        bw.close()
    }

    // TODO: Make this match against all instruction cases and output the
    // relevant assembly code
    def generateLine(instr: Instruction): String =
        s"${instr match {
            case Label(labelName)        => s"$labelName:"
            case Directive(name)         => s".$name"
            case PushInstr(reg)          => s"\tPUSH {${reg.mkString(", ")}}"
            case PopInstr(reg)           => s"\tPOP {${reg.mkString(", ")}}"
            case SubInstr(dst, fst, snd) => s"\tSUB $dst, $fst, $snd"
            case AddInstr(dst, fst, snd) => s"\tADD $dst, $fst, $snd"
            case MoveInstr(dst, src)     => s"\tMOV $dst, $src"
            case LoadLabelInstr(dst, label, cond) => s"\tLDR$cond $dst, =$label"
            case LoadImmIntInstr(dst, imm, cond)  => s"\tLDR$cond $dst, =$imm"
            case LoadInstr(dst, src, ImmOffset(0), cond) =>
                s"\tLDR$cond $dst, [$src]"
            case LoadInstr(dst, src, ImmOffset(offset), cond) =>
                s"\tLDR$cond $dst, [$src, #$offset]"
            case StoreInstr(src, dst, ImmOffset(0)) => s"\tSTR $src, [$dst]"
            case StoreInstr(src, dst, ImmOffset(offset)) =>
                s"\tSTR $src, [$dst, #$offset]"
            case StoreByteInstr(src, dst, ImmOffset(0)) =>
                s"\tSTRB $src, [$dst]"
            case StoreByteInstr(src, dst, ImmOffset(offset)) =>
                s"\tSTRB $src, [$dst, #$offset]"
            case BranchInstr(label, condition)     => s"\tB$condition $label"
            case BranchLinkInstr(label, condition) => s"\tBL$condition $label"
            case CompareInstr(fstOp, sndOp, condition) =>
                s"\tCMP$condition $fstOp, $sndOp"
            case _ => "Temp"
        }}\n"
}
