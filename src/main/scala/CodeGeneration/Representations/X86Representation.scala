import java.io.{File, BufferedWriter, FileWriter}

object X86Representation extends Representation {
    implicit val repr: Representation = this

    def apply(progNode: ProgramNode, st: SymbolTable, filename: String): Unit = {
        implicit val collector: WaccBuffer = new WaccBuffer
        collector.setupMain()
        val bw = new BufferedWriter(new FileWriter(new File(filename)))
        CodeGenerator(progNode, st).foreach(l => bw.write(generateLine(l)))
        bw.close()
    }

    def generateLine(instr: Instruction)(implicit collector: WaccBuffer, repr: Representation): String = 
        s"${instr match {
            case Label(labelName) => s"$labelName:"
            case Directive(name)  => s".$name"
            case PushInstr(_)     => generatePush(instr)
            case PopInstr(_)      => generatePop(instr)
            case MoveInstr(_, _, _) => generateMove(instr)

            /* Logical Instructions */
            case AndInstr(_, _, _, _, _) | OrInstr(_, _, _, _, _) | XorInstr(_, _, _, _, _) 
                => generateLogicalBinOp(instr)

            /* Arithmetic Instructions*/
            case AddInstr(_, _, _, _)    => generateAdd(instr)
            case SubInstr(_, _, _, _) | ReverseSubInstr(_, _, _, _) => generateSub(instr)
            case SMullInstr(_, _, _, _, _) => "TODO"

            case BranchInstr(_, _) | BranchLinkInstr(_, _) => generateBranch(instr)

            /** Load Instructions*/
            case LoadLabelInstr(_, _, _) | LoadImmIntInstr(_, _, _) | LoadInstr(_, _, _, _) | 
                 LoadRegSignedByte(_, _, _, _) => generateLoad(instr)

            /** Store Instructions */
            case StoreInstr(_, _, _, _) | StoreByteInstr(_, _, _, _) => generateStore(instr)

            /** Comparison Instructions */
            case CompareInstr(_, _, _) => generateCompare(instr)

            case _ => ""
        }}\n"
    

    def generatePush(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case PushInstr(regs) => {
                sb.append(s"\tpush ${regs.head}")
                regs.drop(1).foreach(r => sb.append(s"\n\tpush ${r}"))
                sb.toString
            }
            case _ => ""    
        }
    }
    
    def generatePop(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case PopInstr(regs) => {
                sb.append(s"\tpop ${regs.head}")
                regs.drop(1).foreach(r => sb.append(s"\n\tpop ${r}"))
                sb.toString
            }
            case _ => ""
        }
    }

    def generateLogicalBinOp(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb = new StringBuilder

        i match {
            /** AND */
            case AndInstr(dst, fst, snd, _, Condition.AL) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\tand $fst, $snd")
                sb.toString
            }
            case AndInstr(_, _, _, _, _) => conditionalOp(i)

            /** XOR */
            case XorInstr(dst, fst, snd, _, Condition.AL) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\txor $fst, $snd")
                sb.toString
            }
            case XorInstr(_, _, _, _, _) => conditionalOp(i)

            /** OR */
            case OrInstr(dst, fst, snd, _, Condition.AL) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\tor $fst, $snd")
                sb.toString
            }
            case OrInstr(dst, fst, snd, _, Condition.EQ) => {
                val label: String = s".oreq_${collector.tickGeneral()}:"
                sb.append(s"\tjne $label\n")
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\tor $dst, $snd\n")
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
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\tadd $dst, $snd")
                sb.toString
            }
            case _ => ""
        }
    }

    def generateSub(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case SubInstr(dst, fst, snd, _) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\tsub $dst, $snd")
                sb.toString
            }
            case ReverseSubInstr(dst, fst, snd, _) => {
                sb.append(s"\tmov $dst, $snd\n")
                sb.append(s"\tsub $dst, $fst")
                sb.toString
            }
            case _ => ""
        }
    }

    def generateMove(i: Instruction): String = {
        i match {
            case MoveInstr(dst, src, Condition.AL) => s"\tmov $dst, $src"
            case MoveInstr(dst, src, cond) => s"\tcmov${generateCond(cond)} $dst, $src"
            case _ => "TODO"
        }
    }

    def generateLoad(i: Instruction): String = {
        i match {
            case LoadLabelInstr(dst, label, Condition.AL) => s"\tmov $dst, .$label"
            case LoadImmIntInstr(dst, imm, Condition.AL) => s"\tmov $dst, $imm"
            case LoadInstr(dst, src, ImmOffset(0), Condition.AL)  => s"\tlea $dst, [$src]"
            case LoadInstr(dst, src, ImmOffset(offset), Condition.AL) => s"\tlea $dst, $offset[$src]"
            case LoadRegSignedByte(dst, src, ImmOffset(0), Condition.AL) => s"\tlea $dst, [$src]"
            case LoadRegSignedByte(dst, src, ImmOffset(offset), Condition.AL) => s"\tlea $dst, $offset[$src]"
            case _ => "TODO"
        }
    }

    def generateStore(i: Instruction): String = {
        i match {
            case StoreInstr(src, dst, ImmOffset(0), _) => s"\tmov [$dst], $src"
            case StoreInstr(src, dst, ImmOffset(i), _) => s"\tmov $i[$dst], $src"
            case StoreInstr(src, dst, RegOp(r), _) => s"\tmov [$dst, $r], $src"
            
            case StoreByteInstr(src, dst, ImmOffset(0), _) => s"\tmov [$dst], $src"
            case StoreByteInstr(src, dst, ImmOffset(i), _) => s"\tmov $i[$dst], $src"
            case StoreByteInstr(src, dst, RegOp(r), _) => s"\tmov [$dst, $r], $src"
            case _ => "TODO"
        }
    }

    def generateBranch(i: Instruction): String = {
        i match {
            case BranchInstr(label, Condition.AL) => s"\tjmp .$label"
            case BranchInstr(label, cond) => s"\tj${generateCond(cond)} .$label"
            case BranchLinkInstr(label, Condition.AL) => s"\tcall .$label"
            case _ => ""
        }
    }

    def generateCompare(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder
        
        i match {
            case CompareInstr(fst, snd, Condition.AL) => s"\tcmp $fst, $snd"
            case CompareInstr(fst, snd, Condition.EQ) => {
                val label: String = s".cmpeq_${collector.tickGeneral()}:"
                sb.append(s"\tjne $label\n")
                sb.append(s"\tcmp $fst, $snd")
                sb.append(s"$label:")
                sb.toString
            }
            case CompareInstr(fst, snd, Condition.NE) => {
                val label: String = s".cmpne_${collector.tickGeneral()}:"
                sb.append(s"\tje $label\n")
                sb.append(s"\tcmp $fst, $snd")
                sb.append(s"$label:")
                sb.toString
            }
            case CompareInstr(fst, snd, Condition.GT) => {
                val label: String = s".cmpgt_${collector.tickGeneral()}:"
                sb.append(s"\tjle $label\n")
                sb.append(s"\tcmp $fst, $snd")
                sb.append(s"$label:")
                sb.toString
            }
            case CompareInstr(fst, snd, Condition.GE) => {
                val label: String = s".cmpge_${collector.tickGeneral()}:"
                sb.append(s"\tjl $label\n")
                sb.append(s"\tcmp $fst, $snd")
                sb.append(s"$label:")
                sb.toString
            }
            case CompareInstr(fst, snd, Condition.LT) => {
                val label: String = s".cmplt_${collector.tickGeneral()}:"
                sb.append(s"\tjge $label\n")
                sb.append(s"\tcmp $fst, $snd")
                sb.append(s"$label:")
                sb.toString
            }
            case CompareInstr(fst, snd, Condition.LE) => {
                val label: String = s".cmple_${collector.tickGeneral()}:"
                sb.append(s"\tjg $label\n")
                sb.append(s"\tcmp $fst, $snd")
                sb.append(s"$label:")
                sb.toString
            }
            case CompareInstr(fst, snd, Condition.VS) => {
                val label: String = s".cmpvs_${collector.tickGeneral()}:"
                sb.append(s"\tjno $label\n")
                sb.append(s"\tcmp $fst, $snd")
                sb.append(s"$label:")
                sb.toString
            }
            case _ => ""
        }
    }

    /** X86 condition codes */
    def generateCond(cond: Condition.Condition): String = {
        cond match {
            case Condition.EQ => "e"
            case Condition.NE => "ne"
            case Condition.GT => "g"
            case Condition.GE => "ge"
            case Condition.LT => "l"
            case Condition.LE => "le"
            case Condition.VS => "o"
            case Condition.AL => ""
        }
    }

    /** Complementary condition code, used for conditional execution */
    def generateOppositeCond(cond: Condition.Condition): String = {
        cond match {
            case Condition.EQ => "ne"
            case Condition.NE => "e"
            case Condition.GT => "le"
            case Condition.GE => "l"
            case Condition.LT => "ge"
            case Condition.LE => "g"
            case Condition.VS => "no"
            case Condition.AL => ""
        }
    }

    /** Constructs the labels and jumps for a conditionally executed operation */
    def conditionalOp(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder

        val cond: Condition.Condition = {
            i match {
                case AndInstr(_, _, _, _, cond) => cond
                case XorInstr(_, _, _, _, cond) => cond
                case _ => Condition.AL
            }
        }

        val label: String = {
            i match {
                case AndInstr(_, _, _, _, _) => s".and${cond}_${collector.tickGeneral()}:\n"
                case XorInstr(_, _, _, _, _) => s".xor${cond}_${collector.tickGeneral()}:\n"
                case _ => "TODO"
            }
        }
        sb.append(s"\tj${generateOppositeCond(cond)} $label\n")
        sb.append(opBody(i))
        sb.append(s"\t$label:")
        sb.toString
    }

    /** Constructs the operation body for arguments containing multiple lines. 
      * Used to construct conditional operations in particular. */
    def opBody(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case AndInstr(dst, fst, snd, _, cond) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\tand $dst, $snd\n")
                sb.toString
            }
            case XorInstr(dst, fst, snd, _, cond) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\txor $dst, $snd\n")
                sb.toString
            }
            case _ => "TODO"
        }
    }
}
