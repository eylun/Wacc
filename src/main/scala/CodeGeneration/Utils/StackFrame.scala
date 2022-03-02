import constants._
import Helpers._
import scala.collection.mutable
import scala.collection.immutable.Map

class StackFrame(
    val offsetMap: Map[String, Int],
    val totalBytes: Int,
    val st: SymbolTable
) {
    var tempOffset = 0;
    private val varBytes = {
        var sum = 0
        st.dict.foreach {
            case (k, Variable(t)) => {
                sum += getTypeSize(t)
            }
            case _ =>
        }
        sum
    }

    val head: List[Instruction] = varBytes match {
        case 0 => List.empty
        case _ =>
            List(
              SubInstr(
                sp,
                sp,
                ImmOffset(varBytes),
                false
              )
            )
    }

    val tail: List[Instruction] = varBytes match {
        case 0 => List.empty
        case _ =>
            List(
              AddInstr(
                sp,
                sp,
                ImmOffset(varBytes),
                false
              )
            )
    }

    def addTempOffset(amount: Int): Unit = tempOffset += amount

    def dropTempOffset(amount: Int): Unit = tempOffset -= amount

    def join(sf: StackFrame, st: SymbolTable): StackFrame = {
        val newMap: mutable.Map[String, Int] = mutable.Map[String, Int]()
        offsetMap.foreach {
            case (k, v) => {
                newMap += (k -> (v + sf.totalBytes + tempOffset))
            }
        }
        StackFrame((newMap ++ sf.offsetMap).toMap, sf.totalBytes, st)
    }

    def getOffset(ident: String): Int = {
        offsetMap.get(ident) match {
            case Some(x) => x + tempOffset
            case None =>
                throw new RuntimeException("ident should exist in stack frame")
        }
    }
}
object StackFrame {
    def apply(st: SymbolTable) =
        new StackFrame(generateOffsetMap(st), totalBytes(st), st)

    def apply(offsetMap: Map[String, Int], totalBytes: Int, st: SymbolTable) =
        new StackFrame(offsetMap, totalBytes, st)

    private def totalBytes(st: SymbolTable) = {
        var sum = 0
        st.dict.foreach {
            case ("return", _) => 0
            case (k, v)        => sum += getTypeSize(v)
        }
        sum
    }

    private def generateOffsetMap(st: SymbolTable): Map[String, Int] = {
        var acc = totalBytes(st)
        val map = mutable.Map[String, Int]()
        st.order.foreach {
            /** return is a only for semantic checking */
            case "return" =>
            case k => {
                val v = st.lookup(k).get
                acc -= getTypeSize(v)
                v match {
                    /** Params have to be offset by 4 bytes due to LR being
                      * pushed at the start of a function call
                      */
                    case Param(t) => map += (k -> (acc + WORD_SIZE))
                    case _        => map += (k -> acc)
                }

            }
        }

        /** Convert to immutable map */
        map.toMap
    }

}
