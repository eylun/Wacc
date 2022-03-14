import constants._

object PeepholeOptimisation {
    
    def executePeepholeOptimisation(instrList : List[Instruction]): List[Instruction] = {
        // TODO: call optimisation functions here
        // temporary return value
        List(PopInstr(List(sp)))
    }

}

/** Enumeration for optimisation option flag */
/** Reference:
    O0 = no optimisation (default)
    Oph = peephole optimisation */
object OptimisationFlag extends Enumeration {
    type OptimisationFlag = Value
    val O0, Oph = Value

    val allOptFlags = List("-O0", "-Oph")
}