sealed trait Register

case class StackPtrReg() extends Register {
    override def toString(): String = "sp"
}

case class LinkReg() extends Register {
    override def toString(): String = "lr"
}

case class PCReg() extends Register {
    override def toString(): String = "pc"
}

case class Reg(n: Int) extends Register {
    override def toString(): String = s"r$n"
}

object constants {
    /* General Purpose Registers */
    val r0: Reg = Reg(0)
    val r1: Reg = Reg(1)
    val r2: Reg = Reg(2)
    val r3: Reg = Reg(3)
    val r4: Reg = Reg(4)
    val r5: Reg = Reg(5)
    val r6: Reg = Reg(6)
    val r7: Reg = Reg(7)

    /* Special Purpose Registers */
    val sp: StackPtrReg = StackPtrReg()
    val lr: LinkReg = LinkReg()
    val pc: PCReg = PCReg()
}
