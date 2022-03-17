import constants._
import scala.collection.mutable

object PeepholeOptimisation {

    def executePeepholeOptimisation(instructions: List[Instruction]): List[Instruction] = {
        implicit val instrList: mutable.ListBuffer[Instruction] = instructions.to(mutable.ListBuffer)

        for ((instr, i) <- instrList.zipWithIndex) {
            /* TODO: functions mutate the list given an index */
            removeUnnecessaryLoadAndStore(i)
            removeUnnecessaryPushAndPop(i)
        }

        instrList.toList
    }

    def removeUnnecessaryLoadAndStore(i: Int)(implicit instrList: mutable.ListBuffer[Instruction]): Unit = {
        if (i < instrList.size - 4) {
            (instrList(i), instrList(i + 1)) match {
                case (LoadInstr(loadDst1, _, _, _), StoreInstr(storeSrc1, storeDst1, offset1, _)) => {
                    (instrList(i + 2), instrList(i + 3)) match {
                        case (LoadInstr(`loadDst1`, _, _, _), StoreInstr(`storeSrc1`, `storeDst1`, `offset1`, _)) |
                            (LoadImmIntInstr(`loadDst1`, _, _), StoreInstr(`storeSrc1`, `storeDst1`, `offset1`, _)) => {
                            instrList.remove(i)
                            instrList.remove(i)
                            removeUnnecessaryLoadAndStore(i)
                        }
                        case _ =>
                    }

                }

                case (LoadImmIntInstr(loadDst1, _, _), StoreInstr(storeSrc1, storeDst1, offset1, _)) => {
                    (instrList(i + 2), instrList(i + 3)) match {
                        case (LoadInstr(`loadDst1`, _, _, _), StoreInstr(`storeSrc1`, `storeDst1`, `offset1`, _)) |
                            (LoadImmIntInstr(`loadDst1`, _, _), StoreInstr(`storeSrc1`, `storeDst1`, `offset1`, _)) => {
                            instrList.remove(i)
                            instrList.remove(i)
                            removeUnnecessaryLoadAndStore(i)
                        }
                        case _ =>
                    }

                }
                case (LoadRegSignedByte(loadDst1, _, _, _), StoreByteInstr(storeSrc1, storeDst1, offset1, _)) => {
                    (instrList(i + 2), instrList(i + 3)) match {
                        case (
                              LoadRegSignedByte(`loadDst1`, _, _, _),
                              StoreByteInstr(`storeSrc1`, `storeDst1`, `offset1`, _)
                            ) |
                            (MoveInstr(`loadDst1`, _, _), StoreByteInstr(`storeSrc1`, `storeDst1`, `offset1`, _)) => {
                            instrList.remove(i)
                            instrList.remove(i)
                            removeUnnecessaryLoadAndStore(i)
                        }
                        case _ =>
                    }

                }
                case (MoveInstr(loadDst1, _, _), StoreByteInstr(storeSrc1, storeDst1, offset1, _)) => {
                    (instrList(i + 2), instrList(i + 3)) match {
                        case (
                              LoadRegSignedByte(`loadDst1`, _, _, _),
                              StoreByteInstr(`storeSrc1`, `storeDst1`, `offset1`, _)
                            ) | (
                              MoveInstr(`loadDst1`, _, _),
                              StoreByteInstr(`storeSrc1`, `storeDst1`, `offset1`, _)
                            ) => {
                            instrList.remove(i)
                            instrList.remove(i)
                            removeUnnecessaryLoadAndStore(i)
                        }
                        case _ =>
                    }
                }
                case _ =>
            }
        }
    }

    /** Example of unnnecessary cases:
      *   - Push r1; Pop r1
      *   - Push r1; Push r2; Pop r2; Pop r1
      */
    def removeUnnecessaryPushAndPop(i: Int)(implicit instrList: mutable.ListBuffer[Instruction]): Unit = {
        if (i < instrList.size - 2) {
            (instrList(i), instrList(i + 1)) match {
                case (PushInstr(regList1), PopInstr(regList2)) if regList1 == regList2 => {
                    instrList.remove(i)
                    instrList.remove(i)
                    removeUnnecessaryPushAndPop(i)
                }
                case (PopInstr(regList1), PushInstr(regList2)) if regList1 == regList2 => {
                    instrList.remove(i)
                    instrList.remove(i)
                    removeUnnecessaryPushAndPop(i)
                }
                case _ =>
            }
        }
    }

}

/** Enumeration for optimisation option flag */
/** Reference: O0 = no optimisation (default) ; Oph = peephole optimisation */
/** Peephole Optimisation includes:
  *   - removing unnecessary load and store pairs,
  *   - removing unnecessary push & pop instructions,
  *   - constant folding (binary operations)
  *   - TODO: constant propogation
  */
object OptimisationFlag extends Enumeration {
    type OptimisationFlag = Value
    val O0, Oph = Value

    val allOptFlags = List("-O0", "-Oph")
}
