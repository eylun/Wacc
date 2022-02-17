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
            case Label(labelName)        => s"$labelName:"
            case Directive(name)         => s".$name"
            case PushInstr(reg)          => s"\tPUSH {$reg}"
            case PopInstr(reg)           => s"\tPOP {$reg}"
            case SubInstr(dst, fst, snd) => s"\tSUB $dst $fst $snd"
            case AddInstr(dst, fst, snd) => s"\tADD $dst $fst $snd"
            case MoveInstr(dst, src)     => s"\tMOV $dst $src"
            case _                       => "Temp"
        }}\n"
}
