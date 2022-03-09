import java.io.{File, BufferedWriter, FileWriter}

object X86Representation extends Representation {

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

    def generateLine(instr: Instruction)(implicit collector: WaccBuffer): String = 
        s"${instr match {
            case Label(labelName) => s"$labelName:"
            case Directive(name)  => s".$name"

            case PushInstr(_)     => generatePush(instr)
            case PopInstr(_)      => generatePop(instr)

            case AndInstr(_, _, _, _, _) | OrInstr(_, _, _, _, _) | XorInstr(_, _, _, _, _) 
                => generateLogicalBinOp(instr)

            case AddInstr(_, _, _, _)    => generateAdd(instr)
            case SubInstr(_, _, _, _) | ReverseSubInstr(_, _, _, _) => generateSub(instr)

            case BranchInstr(_, _)       => generateBranch(instr)
            
            case _ => ""
        }}\n"
    

    def generatePush(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case PushInstr(regs) => {
                sb.append(s"\tpushl ${regs.head}")
                regs.drop(1).foreach(r => sb.append(s"\n\tpushl ${r}"))
                sb.toString
            }
            case _ => ""    
        }
    }
    
    def generatePop(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case PopInstr(regs) => {
                sb.append(s"\tpopl ${regs.head}")
                regs.drop(1).foreach(r => sb.append(s"\n\tpopl ${r}"))
                sb.toString
            }
            case _ => ""
        }
    }

    def generateLogicalBinOp(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb = new StringBuilder

        i match {
            case AndInstr(dst, fst, snd, _, Condition.AL) => {
                sb.append(s"\tmovl $dst, $fst\n")
                sb.append(s"\tandl $fst, $snd")
                sb.toString
            }
            case AndInstr(dst, fst, snd, _, Condition.EQ) => {
                val label: String = s"andeq_${collector.tickGeneral()}:"
                sb.append(s"\tcmpl $fst, $snd\n")
                sb.append(s"\tjne $label\n")
                sb.append(s"\tmovl $dst, $fst\n")
                sb.append(s"\tandl $dst, $snd\n")
                sb.append(s"$label:\n")
                sb.toString
            }
            case XorInstr(dst, fst, snd, _, Condition.AL) => {
                sb.append(s"\tmovl $dst, $fst\n")
                sb.append(s"\txorl $fst, $snd")
                sb.toString
            }
            case XorInstr(dst, fst, snd, _, Condition.EQ) => {
                val label: String = s"xoreq_${collector.tickGeneral()}:"
                sb.append(s"\tcmpl $fst, $snd\n")
                sb.append(s"\tjne $label\n")
                sb.append(s"\tmovl $dst, $fst\n")
                sb.append(s"\txorl $dst, $snd\n")
                sb.append(s"$label:\n")
                sb.toString
            }
            case OrInstr(dst, fst, snd, _, Condition.AL) => {
                sb.append(s"\tmovl $dst, $fst\n")
                sb.append(s"\torl $fst, $snd")
                sb.toString
            }
            case OrInstr(dst, fst, snd, _, Condition.EQ) => {
                val label: String = s"oreq_${collector.tickGeneral()}:"
                sb.append(s"\tcmpl $fst, $snd\n")
                sb.append(s"\tjne $label\n")
                sb.append(s"\tmovl $dst, $fst\n")
                sb.append(s"\torl $dst, $snd\n")
                sb.append(s"$label:\n")
                sb.toString
            }
            case _ => ""
        }
    }

    def generateAdd(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case AddInstr(dst, fst, snd, _) => {
                sb.append(s"\tmovl $dst, $fst\n")
                sb.append(s"\taddl $dst, $snd")
                sb.toString
            }
            case _ => ""
        }
    }

    def generateSub(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case SubInstr(dst, fst, snd, _) => {
                sb.append(s"\tmovl $dst, $fst\n")
                sb.append(s"\tsubl $dst, $snd")
                sb.toString
            }
            case ReverseSubInstr(dst, fst, snd, _) => {
                sb.append(s"\tmovl $dst, $snd\n")
                sb.append(s"\tsubl $dst, $fst")
                sb.toString
            }
            case _ => ""
        }
    }

    def generateMove(i: Instruction): String = {
        i match {
            case MoveInstr(dst, src, Condition.AL) => s"\tmovl $dst, $src"
            case MoveInstr(dst, src, Condition.EQ) => s"\tcmove $dst, $src"
            case MoveInstr(dst, src, Condition.NE) => s"\tcmovne $dst, $src"
            case MoveInstr(dst, src, Condition.GT) => s"\tcmovg $dst, $src"
            case MoveInstr(dst, src, Condition.GE) => s"\tcmovge $dst, $src"
            case MoveInstr(dst, src, Condition.LT) => s"\tcmovl $dst, $src"
            case MoveInstr(dst, src, Condition.LE) => s"\tcmovle $dst, $src"
            case _ => ""
        }
    }

    def generateBranch(i: Instruction): String = {
        i match {
            case BranchInstr(label, Condition.AL) => s"\tjmp $label"
            case BranchInstr(label, Condition.EQ) => s"\tje $label"
            case BranchInstr(label, Condition.NE) => s"\tjne $label"
            case BranchInstr(label, Condition.GT) => s"\tjg $label"
            case BranchInstr(label, Condition.GE) => s"\tjge $label"
            case BranchInstr(label, Condition.LT) => s"\tjl $label"
            case BranchInstr(label, Condition.LE) => s"\tjle $label"
            case BranchInstr(label, Condition.VS) => s"\tjo $label"

            case _ => ""
        }
    }

    def generateCompare(i: Instruction)(implicit collector: WaccBuffer): String = {
        i match {
            case CompareInstr(fst, snd, Condition.AL) => s"\tcmpl $fst, $snd"
            case CompareInstr(fst, snd, Condition.EQ) => {
                val label: String = s"cmpeq_${collector.tickGeneral()}:"
                val sb: StringBuilder = new StringBuilder
                sb.append(s"\tjne $label\n")
                sb.append(s"\tcmpl $fst, $snd")
                sb.append(s"$label:")
                sb.toString
            }
            case _ => ""
        }
    }
}
