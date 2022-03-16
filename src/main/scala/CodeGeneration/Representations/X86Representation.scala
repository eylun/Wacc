import java.io.{File, BufferedWriter, FileWriter}
import constants.Register

object X86Representation extends Representation {
    implicit val repr: Representation = this

    def apply(progNode: ProgramNode, st: SymbolTable, filename: String): Unit = {
        implicit val collector: WaccBuffer = new WaccBuffer
        collector.setupMain()
        val bw = new BufferedWriter(new FileWriter(new File(filename)))
        CodeGenerator(progNode, st).foreach(l => bw.write(generateLine(l)))
        bw.close()
    }

    def generateOperand(op: SecondOperand): String = {
        op match {
            case ImmOffset(immOffset) => s"$immOffset"
            case RegOp(regOp)         => s"$regOp"
            case LSLRegOp(r, s)       => s"TODO LSL"
            case LSRRegOp(r, s)       => s"TODO LSR"
            case ASRRegOp(r, s)       => s"TODO ASR"
            case RORRegOp(r, s)       => s"TODO ROR"
        }
    }

    def generateShift(s: Shift): String = {
        "TODO SHIFT"
    }

    def generateLine(instr: Instruction)(implicit collector: WaccBuffer, repr: Representation): String = {
        s"${instr match {
            case Label(labelName) => s"$labelName:"
            case Directive(name)  => s"\t.$name"
            case PushInstr(_)     => generatePush(instr)
            case PopInstr(_)      => generatePop(instr)
            case MoveInstr(_, _, _) => generateMove(instr)

            /* Logical Instructions */
            case AndInstr(_, _, _, _, _) | OrInstr(_, _, _, _, _) | XorInstr(_, _, _, _, _) 
                => generateLogicalBinOp(instr)

            /* Arithmetic Instructions*/
            case AddInstr(_, _, _, _)    => generateAdd(instr)
            case SubInstr(_, _, _, _) | ReverseSubInstr(_, _, _, _) => generateSub(instr)
            case SMullInstr(_, _, _, _, _) => generateMultiply(instr)

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
    }

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
            case _ => "TODO POP"
        }
    }

    def generateLogicalBinOp(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb = new StringBuilder
        i match {
            /** AND */
            case AndInstr(_, _, _, _, Condition.AL) => opBody(i)
            case AndInstr(_, _, _, _, _) => conditionalOp(i)

            /** XOR */
            case XorInstr(_, _, _, _, Condition.AL) => opBody(i)
            case XorInstr(_, _, _, _, _) => conditionalOp(i)

            /** OR */
            case OrInstr(_, _, _, _, Condition.AL) => opBody(i)
            case OrInstr(_, _, _, _, _) => conditionalOp(i)
            case _ => "TODO LOGICAL BIN OP"
        }
    }

    def generateAdd(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case AddInstr(dst, fst, snd, _) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\tadd $dst, ${generateOperand(snd)}")
                sb.toString
            }
            case _ => "TODO ADD"
        }
    }

    def generateSub(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case SubInstr(dst, fst, snd, _) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\tsub $dst, ${generateOperand(snd)}")
                sb.toString
            }
            case ReverseSubInstr(dst, fst, snd, _) => {
                sb.append(s"\tmov $dst, ${generateOperand(snd)}\n")
                sb.append(s"\tsub $dst, $fst")
                sb.toString
            }
            case _ => "TODO SUB"
        }
    }

    def generateMultiply(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case SMullInstr(rdLo, rdHi, fst, snd, false) => {
                sb.append(s"\timul $rdLo, $fst, $snd\n")
                sb.append(s"\timul ${regWithLength(rdHi, 64)}, $fst, $snd\n")
                sb.append(s"\tshr ${regWithLength(rdHi, 64)}, 32")
                sb.toString
            }
            case _ => "TODO MUL"
        }
    }

    def generateMove(i: Instruction): String = {
        i match {
            case MoveInstr(dst, src, Condition.AL) => s"\tmov $dst, ${generateOperand(src)}"
            case MoveInstr(dst, src, cond) => s"\tcmov${generateCond(cond)} $dst, ${generateOperand(src)}"
            case _ => "TODO MOV"
        }
    }

    def generateLoad(i: Instruction): String = {
        i match {
            case LoadLabelInstr(dst, label, Condition.AL) => s"\tmov $dst, $label"
            case LoadImmIntInstr(dst, imm, Condition.AL) => s"\tmov $dst, $imm"
            case LoadInstr(dst, src, ImmOffset(0), Condition.AL)  => s"\tlea $dst, [$src]"
            case LoadInstr(dst, src, ImmOffset(offset), Condition.AL) => s"\tlea $dst, [$src + $offset]"
            case LoadRegSignedByte(dst, src, ImmOffset(0), Condition.AL) => s"\tlea $dst, [$src]"
            case LoadRegSignedByte(dst, src, ImmOffset(offset), Condition.AL) => s"\tlea $dst, [$src + $offset]"
            case _ => "TODO LOAD"
        }
    }

    def generateStore(i: Instruction): String = {
        i match {
            case StoreInstr(src, dst, ImmOffset(0), _) => s"\tmov [$dst], $src"
            case StoreInstr(src, dst, ImmOffset(i), _) => s"\tmov [$dst + $i], $src"
            case StoreInstr(src, dst, RegOp(r), _) => s"\tmov [$dst + $r], $src"
            
            case StoreByteInstr(src, dst, ImmOffset(0), _) => s"\tmov [$dst], $src"
            case StoreByteInstr(src, dst, ImmOffset(i), _) => s"\tmov $i[$dst], $src"
            case StoreByteInstr(src, dst, RegOp(r), _) => s"\tmov [$dst + $r], $src"
            case _ => "TODO STORE"
        }
    }

    def generateBranch(i: Instruction): String = {
        i match {
            case BranchInstr(label, Condition.AL) => s"\tjmp .$label"
            case BranchInstr(label, cond) => s"\tj${generateCond(cond)} .$label"
            case BranchLinkInstr(label, Condition.AL) => s"\tcall .$label"
            case _ => "TODO BRANCH"
        }
    }

    def generateCompare(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case CompareInstr(fst, snd, Condition.AL) => opBody(i)
            case CompareInstr(_, _, _) => conditionalOp(i)
            case _ => "TODO CMP"
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
            case _ => "TODO COND"
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
            case _ => "TODO REV COND"
        }
    }

    /** Constructs the labels and jumps for a conditionally executed operation */
    def conditionalOp(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder

        val cond: Condition.Condition = {
            i match {
                case AndInstr(_, _, _, _, c) => c
                case OrInstr(_, _, _, _, c) => c
                case XorInstr(_, _, _, _, c) => c
                case CompareInstr(_, _, c) => c
                case _ => Condition.AL
            }
        }

        val labelNo = collector.tickGeneral()

        val label: String = {
            i match {
                case AndInstr(_, _, _, _, _) => s".and${cond}_${labelNo}:"
                case OrInstr(_, _, _, _, _) => s".or${cond}_${labelNo}"
                case XorInstr(_, _, _, _, _) => s".xor${cond}_${labelNo}:"
                case CompareInstr(_, _, _) => s".cmp${cond}_${labelNo}:"
                case _ => "TODO COND LABEL"
            }
        }
        sb.append(s"\tj${generateOppositeCond(cond)} $label\n")
        sb.append(opBody(i))
        sb.append("\n")
        sb.append(s"\t$label:")
        sb.toString
    }

    /** Constructs the operation body for arguments containing multiple lines. 
      * Used to construct conditional operations in particular. */
    def opBody(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case AndInstr(dst, fst, snd, _, _) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\tand $dst, ${generateOperand(snd)}")
                sb.toString
            }
            case OrInstr(dst, fst, snd, _, _) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\tor $fst, ${generateOperand(snd)}")
                sb.toString
            }
            case XorInstr(dst, fst, snd, _, _) => {
                sb.append(s"\tmov $dst, $fst\n")
                sb.append(s"\txor $dst, ${generateOperand(snd)}")
                sb.toString
            }
            case CompareInstr(fst, snd, _) => {
                sb.append(s"\tcmp $fst, ${generateOperand(snd)}")
                sb.toString
            }
            case _ => "TODO OPBODY"
        }
    }

    /** Returns x86 register representation in n-bit mode (default toString is the 32-bit mode) */
    def regWithLength(r: Register, bitLen: Int): String = {
        bitLen match {
            case 16 => s"${r.toString.substring(1)}"
            case 32 => s"$r"
            case 64 => s"r${r.toString.substring(1)}"
            case _ => "undefined register length in x86"
        }
    }
}
