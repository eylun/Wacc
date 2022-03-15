import constants.Register

sealed trait Instruction

/** Enumerations: Condition Codes, Flags */
object Condition extends Enumeration {
    type Condition = Value
    val EQ, NE, LE, LT, GE, GT, HS, LO, MI, PL, VS, VC, HI, LS, CS = Value
    val AL = Value("")
}

/** Label
  *
  * Labels are in the form <label>:
  */
case class Label(labelName: String) extends Instruction

/** Directive
  *
  * Directives are in the form .<directive>
  */
case class Directive(name: String) extends Instruction

/** COMPARE INSTRUCTION */
/** Compares the value in fstOp with sndOp. Updates condition flags on the
  * result
  */
case class CompareInstr(
    fstOp: Register,
    sndOp: SecondOperand,
    condition: Condition.Condition = Condition.AL
) extends Instruction

/** ARITHMETIC OPERATION INSTRUCTIONS */
/** Results from the artihmetic operation are stored in dst */

/** Adds value in fstOp and sndOp */
case class AddInstr(
    dst: Register,
    fstOp: Register,
    sndOp: SecondOperand,
    setFlags: Boolean = false
) extends Instruction

/** Subtracts value of sndOp from the value in fstOp */
case class SubInstr(
    dst: Register,
    fstOp: Register,
    sndOp: SecondOperand,
    setFlags: Boolean = false
) extends Instruction

/** Subtracts value in fstOp from the value of sndOp */
case class ReverseSubInstr(
    dst: Register,
    fstOp: Register,
    sndOp: SecondOperand,
    setFlags: Boolean = false
) extends Instruction

/** Multiplies values in fstOp and sndOp and stores the least significant 32
  * bits of the result in regLo and the most significant 32 bits of the result
  * in RegHi
  */
case class SMullInstr(
    regLo: Register,
    regHi: Register,
    fstOp: Register,
    sndOp: Register,
    setFlags: Boolean = false
) extends Instruction

/** LOAD AND STORE INSTRUCTIONS */
/** Loads value from memory address stored in src into dst register */
case class LoadInstr(
    dst: Register,
    src: Register,
    offset: SecondOperand,
    condition: Condition.Condition = Condition.AL
) extends Instruction

/** Loads immediate value into register */
case class LoadImmIntInstr(
    dst: Register,
    imm: Int,
    condition: Condition.Condition = Condition.AL
) extends Instruction

/** Address of label is placed in a literal pool and the address is loaded into
  * the register
  */
case class LoadLabelInstr(
    dst: Register,
    label: String,
    condition: Condition.Condition = Condition.AL
) extends Instruction

/** Loads a byte from memory, sign-extends it, and writes the result to a
  * register.
  */
case class LoadRegSignedByte(
    dst: Register,
    src: Register,
    offset: SecondOperand,
    condition: Condition.Condition = Condition.AL
) extends Instruction

/** Stores value in src into memory address equal to sum of dst and offset */
case class StoreInstr(
    src: Register,
    dst: Register,
    offset: SecondOperand,
    writeBack: Boolean = false
) extends Instruction

/** Stores a byte from src into memory address equal to sum of dst and offset */
case class StoreByteInstr(
    src: Register,
    dst: Register,
    offset: SecondOperand,
    writeBack: Boolean = false
) extends Instruction

/** MOVE INSTRUCTION */
case class MoveInstr(
    dst: Register,
    src: SecondOperand,
    condition: Condition.Condition = Condition.AL
) extends Instruction

/** BRANCH INSTRUCTIONS */
/** Branches to label */
case class BranchInstr(
    label: String,
    condition: Condition.Condition = Condition.AL
) extends Instruction

/** Branches to label, and copies the address of the next instruction into the
  * Link Register
  */
case class BranchLinkInstr(
    label: String,
    condition: Condition.Condition = Condition.AL
) extends Instruction

/** LOGICAL OPERATIONS INSTRUCTIONS */
case class AndInstr(
    dst: Register,
    src: Register,
    op2: SecondOperand,
    setFlags: Boolean = false,
    cond: Condition.Condition = Condition.AL
) extends Instruction

case class XorInstr(
    dst: Register,
    src: Register,
    op2: SecondOperand,
    setFlags: Boolean = false,
    cond: Condition.Condition = Condition.AL
) extends Instruction

case class OrInstr(
    dst: Register,
    src: Register,
    op2: SecondOperand,
    setFlags: Boolean = false,
    cond: Condition.Condition = Condition.AL
) extends Instruction

/** STACK MANIPULATION INSTRUCTIONS */
case class PushInstr(regList: List[Register]) extends Instruction

case class PopInstr(regList: List[Register]) extends Instruction

/** Second Operand */
sealed trait SecondOperand

/** Immediate offset */
case class ImmOffset(immOffset: Int) extends SecondOperand

/** Register */
case class RegOp(regOp: Register) extends SecondOperand

/** Logical Shift Left (Register) */
case class LSLRegOp(regOp: Register, shift: Shift) extends SecondOperand

/** Logical Shift Right (Register) */
case class LSRRegOp(regOp: Register, shift: Shift) extends SecondOperand

/** Arithmetic Shift Right (Register) */
case class ASRRegOp(regOp: Register, shift: Shift) extends SecondOperand

/** Rotate Right (Register) */
case class RORRegOp(regOp: Register, shift: Shift) extends SecondOperand

sealed trait Shift

/** Shift by register value */
case class ShiftReg(reg: Register) extends Shift

/** Shift by immediate offset */
case class ShiftImm(imm: Int) extends Shift
