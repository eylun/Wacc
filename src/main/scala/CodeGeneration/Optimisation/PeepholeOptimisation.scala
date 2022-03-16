import constants._
import scala.collection.mutable

object PeepholeOptimisation {

    def executePeepholeOptimisation(instructions: List[Instruction]): List[Instruction] = {
        implicit val instrList: mutable.ListBuffer[Instruction] = instructions.to(mutable.ListBuffer)

        for ((instr, i) <- instrList.zipWithIndex) {
            /* TODO: functions mutate the list given an index */
            removeUnnecessaryLoadAndStore(i)

            /** removeUnnecessaryStores(i) removeUnnecessaryPushAndPop(i)
              */
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

}

/** Enumeration for optimisation option flag */
/** Reference: O0 = no optimisation (default) Oph = peephole optimisation
  */
object OptimisationFlag extends Enumeration {
    type OptimisationFlag = Value
    val O0, Oph = Value

    val allOptFlags = List("-O0", "-Oph")
}
