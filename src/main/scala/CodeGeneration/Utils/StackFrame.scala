import constants._
import Helpers._
import scala.collection.mutable
import scala.collection.immutable.Map

class StackFrame(
    val childOffsetMap: Map[String, Int],
    val parentOffsetMap: Map[String, Int],
    val totalBytes: Int,
    val currST: SymbolTable,
    val returnOffset: Int
) {
    var tempOffset = 0;
    val varBytes = StackFrame.varBytes(childOffsetMap, currST)
    var unlocked = mutable.Set[String]().empty

    /** Decrement stack pointer */
    val head: List[Instruction] = varBytes match {
        case 0 => List.empty
        case _ =>
            StackFrame
                .splitOffsets(varBytes)
                .map(n => SubInstr(sp, sp, ImmOffset(n), false))
    }

    /** Increment stack pointer */
    val tail: List[Instruction] = varBytes match {
        case 0 => List.empty
        case _ =>
            StackFrame
                .splitOffsets(varBytes)
                .map(n => AddInstr(sp, sp, ImmOffset(n), false))
    }

    /** Special case for return statements */
    val returnTail: List[Instruction] = returnOffset match {
        case 0 => List.empty
        case _ =>
            StackFrame
                .splitOffsets(returnOffset)
                .map(n => AddInstr(sp, sp, ImmOffset(n), false))
    }

    def addTempOffset(amount: Int): Unit = tempOffset += amount

    def dropTempOffset(amount: Int): Unit = tempOffset -= amount

    /** Join a given stack frame to this stack frame */
    def join(st: SymbolTable): StackFrame = {
        val newParentMap: mutable.Map[String, Int] = mutable.Map[String, Int]()
        parentOffsetMap.foreach {
            case (k, v) => {
                newParentMap += (k -> (v + StackFrame.totalBytes(
                  st
                ) + tempOffset))
            }
        }
        unlocked.foreach { k =>
            {
                childOffsetMap.get(k) match {
                    case Some(x) =>
                        newParentMap += (k -> (x + StackFrame.totalBytes(
                          st
                        ) + tempOffset))
                    case None =>
                }
            }
        }
        val newChildMap = StackFrame.generateOffsetMap(st)
        // println("join...")
        // println(s"new child map: $newChildMap")
        // println(s"old child map: $childOffsetMap")
        // println(s"new parent map: ${newParentMap.toMap}")
        // println(s"old parent map: $parentOffsetMap")
        StackFrame(
          newChildMap,
          newParentMap.toMap,
          StackFrame.totalBytes(st),
          st,
          varBytes + StackFrame.varBytes(newChildMap, st)
        )
    }

    /** Get offset corresponding to an identifier existing in the stack frame */
    def getOffset(ident: String): Int = {
        // println(s"getting $ident")
        if (unlocked.contains(ident)) {
            childOffsetMap.get(ident) match {
                case Some(x) => return x + tempOffset
                case None    =>
            }
        }
        parentOffsetMap.get(ident) match {
            case Some(x) => x + tempOffset
            case None =>
                throw new RuntimeException("ident should exist in stack frame")
        }
    }

    def unlock(ident: String): Unit = {
        // println(s"unlocking $ident")
        unlocked.add(ident)
    }
}
object StackFrame {
    def apply(st: SymbolTable) = {
        val offsetMap = generateOffsetMap(st)
        new StackFrame(
          offsetMap,
          Map.empty,
          totalBytes(st),
          st,
          varBytes(offsetMap, st)
        )
    }

    def apply(
        childOffsetMap: Map[String, Int],
        parentOffsetMap: Map[String, Int],
        totalBytes: Int,
        st: SymbolTable,
        varBytes: Int
    ) =
        new StackFrame(
          childOffsetMap,
          parentOffsetMap,
          totalBytes,
          st,
          varBytes
        )

    /** Total bytes of symbol table */
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
            /** Return case is a only for semantic checking */
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

    /** Split offsets */
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
