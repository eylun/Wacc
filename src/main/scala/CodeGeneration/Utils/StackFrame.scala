import constants._
import Helpers._
import scala.collection.mutable
import scala.collection.immutable.Map

class StackFrame(val offsetMap: Map[String, Int], val totalBytes: Int) {

    private val head: List[Instruction] = totalBytes match {
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

    private val tail: List[Instruction] = totalBytes match {
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

    def emit(instructions: List[Instruction]): List[Instruction] =
        head ++ instructions ++ tail

    def join(sf: StackFrame): StackFrame = {
        val newMap: mutable.Map[String, Int] = mutable.Map[String, Int]()
        offsetMap.foreach { case (k, v) =>
            newMap += (k -> (v + sf.totalBytes))
        }
        StackFrame((newMap ++ sf.offsetMap).toMap, sf.totalBytes)
    }

    def getOffset(ident: String): Int = {
        offsetMap.get(ident) match {
            case Some(x) => x
            case None => 
                throw new RuntimeException("ident should exist in stack frame")
        }
    } 
}
object StackFrame {
    def apply(st: SymbolTable) =
        new StackFrame(generateOffsetMap(st), totalBytes(st))

    def apply(offsetMap: Map[String, Int], totalBytes: Int) =
        new StackFrame(offsetMap, totalBytes)

    private def totalBytes(st: SymbolTable) =
        st.dict.foldLeft(0)((p, n) => p + getTypeSize(n._2))

    private def generateOffsetMap(st: SymbolTable): Map[String, Int] = {
        var acc = 0
        val map = mutable.Map[String, Int]()
        st.dict.foreach {
            case (k, v) => {
                map += (k -> acc)
                acc += getTypeSize(v)
            }
        }

        /** Convert to immutable map */
        map.toMap
    }

}
