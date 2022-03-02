import constants._
import Helpers._
import scala.collection.mutable
import scala.collection.immutable.Map

class StackFrame(
    val offsetMap: Map[String, Int],
    val totalBytes: Int,
    val st: SymbolTable,
    val returnOffset: Int
) {
    var tempOffset = 0;
    val varBytes = StackFrame.varBytes(offsetMap, st)

    val head: List[Instruction] = varBytes match {
        case 0 => List.empty
        case _ =>
            StackFrame
                .splitOffsets(varBytes)
                .map(n => SubInstr(sp, sp, ImmOffset(n), false))
    }

    val returnTail: List[Instruction] = returnOffset match {
        case 0 => List.empty
        case _ =>
            StackFrame
                .splitOffsets(returnOffset)
                .map(n => AddInstr(sp, sp, ImmOffset(n), false))
    }

    val tail: List[Instruction] = varBytes match {
        case 0 => List.empty
        case _ =>
            StackFrame
                .splitOffsets(varBytes)
                .map(n => AddInstr(sp, sp, ImmOffset(n), false))
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
        StackFrame(
          (newMap ++ sf.offsetMap).toMap,
          sf.totalBytes,
          st,
          varBytes + StackFrame.varBytes(sf.offsetMap, st)
        )
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
    def apply(st: SymbolTable) = {
        val offsetMap = generateOffsetMap(st)
        new StackFrame(offsetMap, totalBytes(st), st, varBytes(offsetMap, st))
    }

    def apply(
        offsetMap: Map[String, Int],
        totalBytes: Int,
        st: SymbolTable,
        varBytes: Int
    ) =
        new StackFrame(offsetMap, totalBytes, st, varBytes)

    private def totalBytes(st: SymbolTable) = {
        var sum = 0
        st.dict.foreach {
            case ("return", _) => 0
            case (k, v)        => sum += getTypeSize(v)
        }
        sum
    }

    private def varBytes(offsetMap: Map[String, Int], st: SymbolTable): Int = {
        var sum = 0
        offsetMap.foreach { case (k, _) =>
            st.lookup(k) match {
                case Some(Variable(t)) => sum += getTypeSize(t)
                case _                 =>
            }
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

    private def splitOffsets(offset: Int): List[Int] = {
        import scala.collection.immutable.List
        val lb = mutable.ListBuffer[Int]()
        val chunks = offset / OFFSET_MAX
        val rem = offset % OFFSET_MAX
        for (n <- 0 until chunks) {
            lb += 1024
        }
        lb += rem
        lb.toList
    }

}
