sealed trait Instruction

// TODO: Fill up all of this

/** Label
  *
  * Labels are in the form <label>:
  */
case class Label(labelName: String) extends Instruction {

}

/** Directive
  *
  * Directives are in the form .<directive>
  */
case class Directive(name: String) extends Instruction

/* Arithmetic Operations */
case class AddInstr(dst: Register, fstOp: Register, sndOp: SecondOperand) extends Instruction

case class SubInstr(dst: Register, fstOp: Register, sndOp: SecondOperand) extends Instruction

case class SMullInstr(regLo: Register, regHi: Register, fstOp: Register, sndOp: Register) extends Instruction

case class DivInstr(regLo: Register, regHi: Register, fstOp: Register, sndOp: Register) extends Instruction

/** Loading and Storing Instructions */
/** Load from: memory */
case class LoadInstr(dst: Register, src: Register, offset: SecondOperand) extends Instruction

case class StoreInstr(dst: Register, src: Register, offset: SecondOperand) extends Instruction

case class MoveInstr(dst: Register, src: SecondOperand) extends Instruction

/** Branch Instructions */
case class BranchInstr(label: String, condition: Condition) extends Instruction

case class BranchLinkInstr(label: String) extends Instruction

/** Logic Operations */
case class AndInstr(fstOp: Register, sndOp: Register) extends Instruction

case class XorInstr(fstOp: Register, sndOp: Register) extends Instruction

case class OrInstr(fstOp: Register, sndOp: Register) extends Instruction

/** Comparison Operation */
case class CmpInstr(reg: Register, sndOp: SecondOperand) extends Instruction

/* Stack Manipulation Operation*/
case class PushInstr(reg: Register) extends Instruction

case class PopInstr(reg: Register) extends Instruction


/** Enumerations: Condition Codes, Flags */
object Condition extends Enumeration {
  type Condition = Value
  val EQ, NE, LE, LT, GE, GT, AL = Value
}


/** Second Operand */
sealed trait SecondOperand

case class immOffset(immOffset: Int) extends SecondOperand

case class regOp(regOp: Register) extends SecondOperand


