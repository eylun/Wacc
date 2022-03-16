object constants {
    sealed trait Register

    /** toString functions for registers */
    private case class StackPtrReg()(implicit repr: Representation) extends Register {
        override def toString(): String = {
            repr match {
                case ARMRepresentation => "sp"
                case X86Representation => "%esp"
            }
        }
    }

    private case class LinkReg()(implicit repr: Representation) extends Register {
        override def toString(): String = {
            repr match {
                case ARMRepresentation => "lr"
                case X86Representation => "%ebp"
            }
        }
    }

    private case class PCReg()(implicit repr: Representation) extends Register {
        override def toString(): String = {
            repr match {
                case ARMRepresentation => "pc"
                case X86Representation => "%eip"
            }
        }
    }

    private case class Reg(n: Int)(implicit repr: Representation) extends Register {
        override def toString(): String = {
            repr match {
                case ARMRepresentation => s"r$n"
                case X86Representation => {
                    n match {
                        case 0 => "%eax"
                        case 1 => "%edi"
                        case 2 => "%esi"
                        case 3 => "%edx"
                        case 4 => "%ecx"
                        case 5 => "%e8"
                        case 6 => "%e9"
                        case 7 => "%ebx"
                    }
                }
            }
        }
    }

    /** General Purpose Registers */
    def r0(implicit repr: Representation): Register = Reg(0)
    def r1(implicit repr: Representation): Register = Reg(1)
    def r2(implicit repr: Representation): Register = Reg(2)
    def r3(implicit repr: Representation): Register = Reg(3)
    def r4(implicit repr: Representation): Register = Reg(4)
    def r5(implicit repr: Representation): Register = Reg(5)
    def r6(implicit repr: Representation): Register = Reg(6)
    def r7(implicit repr: Representation): Register = Reg(7)
    

    /** Special Purpose Registers */
    def sp(implicit repr: Representation): Register = StackPtrReg()
    def lr(implicit repr: Representation): Register = LinkReg()
    def pc(implicit repr: Representation): Register = PCReg()
}
