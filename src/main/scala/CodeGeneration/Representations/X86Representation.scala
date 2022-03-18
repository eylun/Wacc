import java.io.{File, BufferedWriter, FileWriter}
import constants._
import scala.collection.immutable.Set

object X86Representation extends Representation {
    implicit val repr: Representation = this
    val externFunc1: Set[String] = Set("exit", "malloc", "free", "putchar", "fflush", "scanf", "puts")
    val externFunc2: Set[String] = Set("__aeabi_idiv", "__aeabi_idivmod", "printf")

    def apply(progNode: ProgramNode, st: SymbolTable, filename: String): Unit = {
        implicit val collector: WaccBuffer = new WaccBuffer
        collector.setupMain()
        val bw = new BufferedWriter(new FileWriter(new File(filename)))
        CodeGenerator(progNode, st).foreach(l => bw.write(generateLine(l)))
        bw.close()
    }

    /** Converts a operand. Register shift is performed this translation, so the shift is ignored. */
    def generateOperand(op: SecondOperand): String = {
        op match {
            case ImmOffset(immOffset) => "$" + s"$immOffset"
            case RegOp(regOp)         => s"$regOp"
            case LSLRegOp(r, _)       => s"$r"
            case LSRRegOp(r, _)       => s"$r"
            case ASRRegOp(r, _)       => s"$r"
            case RORRegOp(r, _)       => s"$r"
            case _                    => "TODO OP2"
        }
    }

    def generateShiftValue(s: Shift): String = {
        s match {
            case ShiftReg(r) => s"$r"
            case ShiftImm(i) => "$" + s"$i"
        }
    }

    def generateShift(op: SecondOperand): String = {
        op match {
            case LSLRegOp(r, s) => s"\tshl ${generateShiftValue(s)}, $r\n"
            case LSRRegOp(r, s) => s"\tshr ${generateShiftValue(s)}, $r\n"
            case ASRRegOp(r, s) => s"\tsar ${generateShiftValue(s)}, $r\n"
            case RORRegOp(r, s) => s"\tror ${generateShiftValue(s)}, $r\n"
            case _              => ""
        }
    }

    def generateLine(instr: Instruction)(implicit collector: WaccBuffer, repr: Representation): String = {
        s"${instr match {
            case Label(labelName)   => s"$labelName:"
            case Directive(s"word$_") => ""
            case Directive(name)    => s"\t.$name"
            case GlobalDirective()  => "\t.globl _start"
            case PushInstr(_)       => generatePush(instr)
            case PopInstr(_)        => generatePop(instr)
            case MoveInstr(_, _, _) => generateMove(instr)

            /* Logical Instructions */
            case AndInstr(_, _, _, _, _) | OrInstr(_, _, _, _, _) | XorInstr(_, _, _, _, _) =>
                generateLogicalBinOp(instr)

            /* Arithmetic Instructions*/
            case AddInstr(_, _, _, _)                               => generateAdd(instr)
            case SubInstr(_, _, _, _) | ReverseSubInstr(_, _, _, _) => generateSub(instr)
            case SMullInstr(_, _, _, _, _)                          => generateMultiply(instr)

            case BranchInstr(_, _) | BranchLinkInstr(_, _) => generateBranch(instr)

            /** Load Instructions */
            case LoadLabelInstr(_, _, _) | LoadImmIntInstr(_, _, _) | LoadInstr(_, _, _, _) | LoadRegSignedByte(_, _, _, _) =>
                generateLoad(instr)

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
                if (regs.head == pc) {
                    sb.append(s"\tlea (${regs.head}), %r9\n")
                    sb.append(s"\tpush %r9")
                } else {
                    sb.append(s"\tpush ${regs.head}")
                }
                regs.drop(1)
                    .foreach(r => {
                        if (r == pc) {
                            sb.append(s"\n\tlea (${regs.head}), %r9")
                            sb.append(s"\n\tpush %r9")
                        } else {
                            sb.append(s"\n\tpush $r")
                        }
                    })
                sb.toString
            }
            case _ => ""
        }
    }

    def generatePop(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case PopInstr(regs) => {
                if (regs.head == pc) {
                    sb.append(s"\tret")
                } else {
                    sb.append(s"\tpop ${regs.head}")
                }
                regs.drop(1)
                    .foreach(r => {
                        if (r == pc) {
                            sb.append(s"\n\tret")
                        } else {
                            sb.append(s"\n\tpop $r")
                        }
                    })
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
            case AndInstr(_, _, _, _, _)            => conditionalOp(i)

            /** XOR */
            case XorInstr(_, _, _, _, Condition.AL) => opBody(i)
            case XorInstr(_, _, _, _, _)            => conditionalOp(i)

            /** OR */
            case OrInstr(_, _, _, _, Condition.AL) => opBody(i)
            case OrInstr(_, _, _, _, _)            => conditionalOp(i)
            case _                                 => "TODO LOGICAL BIN OP"
        }
    }

    def generateAdd(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case AddInstr(dst, fst, snd, _) => {
                sb.append(s"\tmov $fst, $dst\n")
                sb.append(generateShift(snd))
                sb.append(s"\tadd ${generateOperand(snd)}, $dst")
                sb.toString
            }
            case _ => "TODO ADD"
        }
    }

    def generateSub(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case SubInstr(dst, fst, snd, _) => {
                sb.append(s"\tmov $fst, $dst\n")
                sb.append(generateShift(snd))
                sb.append(s"\tsub ${generateOperand(snd)}, $dst")
                sb.toString
            }
            case ReverseSubInstr(dst, fst, snd, _) => {
                sb.append(generateShift(snd))
                sb.append(s"\tmov ${generateOperand(snd)}, $dst\n")
                sb.append(s"\tsub $fst, $dst")
                sb.toString
            }
            case _ => "TODO SUB"
        }
    }

    def generateMultiply(i: Instruction): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case SMullInstr(rdLo, rdHi, fst, snd, false) => {
                sb.append(s"\tmov $fst, $rdLo\n")
                sb.append(s"\timul $snd, $rdLo}\n")
                sb.append(s"\tmov $fst, $rdHi\n")
                sb.append(s"\timul $snd, $rdHi\n")
                sb.append(s"\tshr 32, $rdHi")
                sb.toString
            }
            case _ => "TODO MUL"
        }
    }

    def generateMove(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case MoveInstr(dst, src, Condition.AL) => opBody(i)
            case MoveInstr(_, _, _) => conditionalOp(i)
            case _ => "TODO MOV"
        }
    }

    def generateLoad(i: Instruction)(implicit collector: WaccBuffer): String = {
        i match {
            case LoadLabelInstr(dst, label, Condition.AL)                => s"\tmov $$$label, $dst"
            case LoadLabelInstr(dst, label, cond)                        => conditionalOp(i)
            case LoadImmIntInstr(dst, imm, Condition.AL)                 => s"\tmov $$$imm, $dst"
            case LoadImmIntInstr(dst, imm, cond)                         => conditionalOp(MoveInstr(dst, ImmOffset(imm), cond))
            case LoadInstr(dst, src, ImmOffset(0), Condition.AL)         => s"\tmov ($src), $dst"
            case LoadInstr(dst, src, ImmOffset(ofs), Condition.AL)       => s"\tmov $ofs($src), $dst"
            case LoadRegSignedByte(dst, src, ImmOffset(0), Condition.AL) => s"\tmov ($src), $dst"
            case LoadRegSignedByte(dst, src, ImmOffset(ofs), Condition.AL) => s"\tmov $ofs($src), $dst"
            case _                                                         => s"TODO LOAD: $i"
        }
    }

    def generateStore(i: Instruction): String = {
        i match {
            case StoreInstr(src, dst, ImmOffset(0), _) => s"\tmov $src, ($dst)"
            case StoreInstr(src, dst, ImmOffset(i), _) => s"\tmov $src, $i($dst)"
            case StoreInstr(src, dst, RegOp(r), _)     => s"\tmov $src, ($dst,$r)"

            case StoreByteInstr(src, dst, ImmOffset(0), _) => s"\tmov $src, ($dst)"
            case StoreByteInstr(src, dst, ImmOffset(i), _) => s"\tmov $src, $i($dst)"
            case StoreByteInstr(src, dst, RegOp(r), _)     => s"\tmov $src, ($dst,$r)"
            case _                                         => "TODO STORE"
        }
    }

    def generateBranch(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case BranchInstr(label, Condition.AL)     => s"\tjmp $label"
            case BranchInstr(label, cond)             => s"\tj${generateCond(cond)} $label"
            // case BranchLinkInstr(label, Condition.AL) if externFunc1.contains(label) => {
            //     sb.append("\tmovl %eax, %edi\n")
            //     sb.append(opBody(i))
            //     sb.toString
            // }
            // case BranchLinkInstr(label, Condition.AL) if externFunc2.contains(label) => {
            //     sb.append("\tmovl %eax, %edi\n")
            //     sb.append("\tmovl %ecx, %esi\n")
            //     sb.append(opBody(i))
            //     sb.toString
            // } 
            case BranchLinkInstr(label, Condition.AL) => opBody(i)
            case BranchLinkInstr(label, _)            => conditionalOp(i)
            case _                                    => s"TODO BRANCH: $i"
        }
    }

    def generateCompare(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case CompareInstr(fst, snd, Condition.AL) => opBody(i)
            case CompareInstr(_, _, _)                => conditionalOp(i)
            case _                                    => "TODO CMP"
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
            case _            => "TODO COND"
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
            case _            => "TODO REV COND"
        }
    }

    /** Constructs the labels and jumps for a conditionally executed operation not natively supported in x86 */
    def conditionalOp(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder

        val cond: Condition.Condition = {
            i match {
                case AndInstr(_, _, _, _, c) => c
                case OrInstr(_, _, _, _, c)  => c
                case XorInstr(_, _, _, _, c) => c
                case CompareInstr(_, _, c)   => c
                case BranchLinkInstr(_, c)   => c
                case MoveInstr(_, _, c)      => c
                case LoadLabelInstr(_, _, c) => c
                case _                       => Condition.AL
            }
        }

        val labelNo = collector.tickGeneral()

        val label: String = {
            i match {
                case AndInstr(_, _, _, _, _) => s"and${cond}_${labelNo}"
                case OrInstr(_, _, _, _, _)  => s"or${cond}_${labelNo}"
                case XorInstr(_, _, _, _, _) => s"xor${cond}_${labelNo}"
                case CompareInstr(_, _, _)   => s"cmp${cond}_${labelNo}"
                case BranchLinkInstr(_, _)   => s"bl${cond}_${labelNo}"
                case MoveInstr(_, _, _) | LoadLabelInstr(_, _, _) => s"mov${cond}_${labelNo}"
                case _                       => "TODO COND LABEL"
            }
        }
        sb.append(s"\tj${generateOppositeCond(cond)} $label\n")
        sb.append(opBody(i))
        sb.append("\n")
        sb.append(s"$label:")
        sb.toString
    }

    /** Constructs the operation body for arguments containing multiple lines. Used to construct conditional operations
      * in particular in conjunction with conditionalOp()
      */
    def opBody(i: Instruction)(implicit collector: WaccBuffer): String = {
        val sb: StringBuilder = new StringBuilder
        i match {
            case AndInstr(dst, fst, snd, _, _) => {
                sb.append(s"\tmov $fst, $dst\n")
                sb.append(generateShift(snd))
                sb.append(s"\tand ${generateOperand(snd)}, $dst")
                sb.toString
            }
            case OrInstr(dst, fst, snd, _, _) => {
                sb.append(s"\tmov $fst, $dst\n")
                sb.append(generateShift(snd))
                sb.append(s"\tor ${generateOperand(snd)}, $dst")
                sb.toString
            }
            case XorInstr(dst, fst, snd, _, _) => {
                sb.append(s"\tmov $fst, $dst\n")
                sb.append(generateShift(snd))
                sb.append(s"\txor ${generateOperand(snd)}, $dst")
                sb.toString
            }
            case CompareInstr(fst, snd, _) => {
                sb.append(generateShift(snd))
                sb.append(s"\tcmp ${generateOperand(snd)}, $fst")
                sb.toString
            }
            case BranchLinkInstr(label, _) => {
                sb.append(s"\tcall $label")
                sb.toString
            }
            case MoveInstr(dst, src, _) => {
                sb.append(generateShift(src))
                sb.append(s"\tmov ${generateOperand(src)}, $dst")
                sb.toString
            }
            case LoadLabelInstr(dst, label, _) => {
                s"\tmov $$$label, $dst"
            }
            case _ => "TODO OPBODY"
        }
    }

    // /** Returns x86 register representation in n-bit mode (default toString is the 32-bit mode) */
    // def regWithLength(r: Register, bitLen: Int): String = {
    //     bitLen match {
    //         case 16 => s"%${r.toString.substring(2)}"
    //         case 32 => s"$r"
    //         case 64 => s"%r${r.toString.substring(2)}"
    //         case _  => "undefined register length in x86"
    //     }
    // }
}
