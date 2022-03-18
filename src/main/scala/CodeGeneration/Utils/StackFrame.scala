import constants._
import Helpers._
import scala.collection.mutable
import scala.collection.immutable.Map

/** Stack Frame
  *
  * The stack frame contains two Maps, one Set and the corresponding SymbolTable of the scope that is meant for the
  * stack frame.
  *
  * childOffsetMap: The map containing variables for the current scope.
  *
  * parentOffsetMap: The map containing variables for parent scopes. Note that this would be an empty Map for the main
  * scope
  *
  * unlocked: The set contains identifier strings that tell the stackframe if a certain variable has been reached in the
  * codegen
  *
  * currST: The symbol table that contains information about the variables and their types of the current scope.
  *
  * returnOffset: Used purely for functional returns as a special case.
  */
class StackFrame(
    val childOffsetMap: Map[String, Int],
    val parentOffsetMap: Map[String, Int],
    val currST: SymbolTable,
    val returnOffset: Int
) {
    var unlocked = mutable.Set[String]().empty

    /** temporary offsets are used when something is pushed to the stackframe in order to implement a functionality.
      * These are added to values retrieved from the offsetmaps
      */
    var tempOffset = 0;

    /** varBytes contains the total bytes of the VARIABLES in the current offsetmap. These do not include function
      * parameters
      */
    val varBytes = StackFrame.varBytes(currST)

    /** totalBytes contains the total bytes of all values in the current offsetmap. This includes function parameters
      */
    val totalBytes = StackFrame.totalBytes(currST)

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

    /** Increment stack pointer by all allocated variables in a function */
    val returnTail: List[Instruction] = returnOffset match {
        case 0 => List.empty
        case _ =>
            StackFrame
                .splitOffsets(returnOffset)
                .map(n => AddInstr(sp, sp, ImmOffset(n), false))
    }

    /** Add temporary offset when push instructions are used */
    def addTempOffset(amount: Int): Unit = tempOffset += amount

    /** Drop temporary offset when pop instructions are used */
    def dropTempOffset(amount: Int): Unit = tempOffset -= amount

    /** Join a symbol table to this stack frame */
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

        StackFrame(
          newChildMap,
          newParentMap.toMap,
          st,
          varBytes + StackFrame.varBytes(st)
        )
    }

    /** Get offset corresponding to an identifier existing in the stack frame
      *
      * Attempt to retrieve an offset of an identifier from the child offset map if the identifier is unlocked. If not,
      * attempt to retrieve it from the parent offset map
      *
      * It is expected that the identifier is inside at least one of the maps, if not it means that the semantic check
      * did not work
      */
    def getOffset(ident: String): Int = {
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

    /** Unlock an identifier for the stackframe. This should only be used when declaring a new variable or adding in
      * function parameters
      */
    def unlock(ident: String): Unit = unlocked.add(ident)
}
object StackFrame {

    /** Used for declaring a new main stackframe */
    def apply(st: SymbolTable) = {
        val offsetMap = generateOffsetMap(st)
        new StackFrame(
          offsetMap,
          Map.empty,
          st,
          varBytes(st)
        )
    }

    /** Used for declaring a child stackframe */
    def apply(
        childOffsetMap: Map[String, Int],
        parentOffsetMap: Map[String, Int],
        st: SymbolTable,
        varBytes: Int
    ) =
        new StackFrame(
          childOffsetMap,
          parentOffsetMap,
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

    /** Total bytes of variables in a symbol table */
    private def varBytes(st: SymbolTable): Int = {
        var sum = 0
        st.dict.foreach {
            case (_, Variable(t)) => sum += getTypeSize(t)
            case _                =>
        }
        sum
    }

    /** Generates an offset map based on the provided symbol table */
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
                    /** Params have to be offset by 4 bytes due to LR being pushed at the start of a function call
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
