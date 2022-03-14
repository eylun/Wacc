object constants {
    sealed trait Register

    /** toString functions for registers */
    private case class StackPtrReg()(implicit repr: Representation) extends Register {
        override def toString(): String = {
            repr match {
                case ARMRepresentation => "sp"
                case X86Representation => "rsp"
            }
        }
    }

    private case class LinkReg()(implicit repr: Representation) extends Register {
        override def toString(): String = {
            repr match {
                case ARMRepresentation => "lr"
                case X86Representation => "rbp"
            }
        }
    }

    private case class PCReg()(implicit repr: Representation) extends Register {
        override def toString(): String = {
            repr match {
                case ARMRepresentation => "pc"
                case X86Representation => "not reached"
            }
        }
    }

    private case class Reg(n: Int)(implicit repr: Representation) extends Register {
        override def toString(): String = {
            repr match {
                case ARMRepresentation => s"r$n"
                case X86Representation => {
                    n match {
                        case 0 => "rax"
                        case 1 => "rdi"
                        case 2 => "rsi"
                        case 3 => "rdx"
                        case 4 => "rcx"
                        case 5 => "r8"
                        case 6 => "r9"
                        case 7 => "rbx"
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
