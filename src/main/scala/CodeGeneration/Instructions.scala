sealed trait Instruction

// TODO: Fill up all of this

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

/* Arithmetic Operations */
case class AddInstr() extends Instruction

case class SubInstr() extends Instruction

case class SMullInstr() extends Instruction

case class DivInstr() extends Instruction

case class LoadInstr() extends Instruction

/** Loading and Storing Instructions */
case class StoreInstr() extends Instruction

case class MoveInstr() extends Instruction

/** Branch Instructions */
case class BranchInstr() extends Instruction

case class BranchLinkInstr() extends Instruction

/** Logic Operations */
case class AndInstr() extends Instruction

case class XorInstr() extends Instruction

case class OrInstr() extends Instruction

/* Comparison Operation */
case class CmpInstr() extends Instruction

/* Stack Manipulation Operation*/
case class PushInstr() extends Instruction

case class PopInstr() extends Instruction
