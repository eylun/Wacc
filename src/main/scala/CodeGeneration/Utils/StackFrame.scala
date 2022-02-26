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

    val head: List[Instruction] = totalBytes match {
        case 0 => List.empty
        case _ =>
            List(
              SubInstr(
                sp,
                sp,
                ImmOffset(totalBytes)
              )
            )
    }

    val tail: List[Instruction] = totalBytes match {
        case 0 => List.empty
        case _ =>
            List(
              AddInstr(
                sp,
                sp,
                ImmOffset(totalBytes)
              )
            )
    }

    def addTempOffset(amount: Int): Unit = tempOffset += amount

    def dropTempOffset(amount: Int): Unit = tempOffset -= amount

    def join(sf: StackFrame, st: SymbolTable): StackFrame = {
        val newMap: mutable.Map[String, Int] = mutable.Map[String, Int]()
        offsetMap.foreach {
            case (k, v) => {
                newMap += (k -> (v + sf.totalBytes))
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

    private def totalBytes(st: SymbolTable) =
        st.dict.foldLeft(0)((p, n) => p + getTypeSize(n._2))

    private def generateOffsetMap(st: SymbolTable): Map[String, Int] = {
        var acc = totalBytes(st)
        val map = mutable.Map[String, Int]()
        st.dict.foreach {
            /** return is a only for semantic checking */
            case ("return", _) =>
            case (k, v) => {
                acc -= getTypeSize(v)
                map += (k -> acc)
            }
        }

        /** Convert to immutable map */
        map.toMap
    }

}
