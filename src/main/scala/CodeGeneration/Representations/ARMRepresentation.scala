import java.io.{File, BufferedWriter, FileWriter}
object ARMRepresentation extends Representation {

    implicit val collector: WaccBuffer = new WaccBuffer

    def apply(
        progNode: ProgramNode,
        st: SymbolTable,
        filename: String
    ): Unit = {
        val bw = new BufferedWriter(new FileWriter(new File(filename)))
        CodeGenerator(progNode, st).foreach(l => bw.write(generateLine(l)))
        bw.close()
    }

    // TODO: Make this match against all instruction cases and output the
    // relevant assembly code
    def generateLine(instr: Instruction): String =
        s"${instr match {
            case Label(labelName)                  => s"$labelName:"
            case Directive(name)                   => s".$name"
            case PushInstr(reg)                    => s"\tPUSH {$reg}"
            case PopInstr(reg)                     => s"\tPOP {$reg}"
            case SubInstr(dst, fst, snd, true)     => s"\tSUBS $dst $fst $snd"
            case SubInstr(dst, fst, snd, false)    => s"\tSUB $dst $fst $snd"
            case AddInstr(dst, fst, snd, true)     => s"\tADDS $dst $fst $snd"
            case AddInstr(dst, fst, snd, false)    => s"\tADDS $dst $fst $snd"
            case MoveInstr(dst, src)               => s"\tMOV $dst $src"
            case LoadImmLabelInstr(dst, label)     => s"\tLDR $dst =$label"
            case LoadImmIntInstr(dst, imm)         => s"\tLDR $dst =$imm"
            case LoadInstr(dst, src, ImmOffset(0)) => s"\tLDR $dst [$src]"
            case LoadInstr(dst, src, ImmOffset(offset)) =>
                s"\tLDR $dst [$src, #$offset]"
            case StoreInstr(src, dst, ImmOffset(0)) => s"\tSTR $src [$dst]"
            case StoreInstr(src, dst, ImmOffset(offset)) =>
                s"\tSTR $src [$dst, #$offset]"
            case _ => "Temp"
        }}\n"
}
