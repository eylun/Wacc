sealed trait Register

case class StackPtrReg() extends Register

case class LinkReg() extends Register

case class PCReg() extends Register

case class Reg(n: Int) extends Register

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
