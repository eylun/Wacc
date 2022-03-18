import scala.collection.immutable.Map

class SymbolTable(
    var encSymTable: Option[SymbolTable],
    var dict: Map[String, Identifier]
) {

    var order: List[String] = List.empty
    var constantIntsMap: Map[String, Int] = Map[String, Int]().empty
    var constantBoolsMap: Map[String, Boolean] = Map[String, Boolean]().empty

    /** TODO: removeConstants is a flag that when true, will cause */
    var removeConstants: Boolean = false
    def setToRemove(): Unit = {
        removeConstants = true
    }

    /** Sets the parent of this symbol table. */
    def setParent(parent: SymbolTable): Unit = encSymTable = Some(parent)

    /** Adds an identifier string to the current symbol table, mapped to an identifier object.
      */
    def add(name: String, obj: Identifier): Unit = {
        dict = dict + (name -> obj)
        order ++= List(name)
    }

    /** Looks up an identifier string in the current symbol table. Returns a Some() object containing the identifier
      * object if it exists, otherwise returns None.
      */
    def lookup(name: String): Option[Identifier] = {
        dict get name
    }

    def lookupAll(name: String): Option[Identifier] = {
        var s: Option[SymbolTable] = Some(this)
        while (s != None) {
            var st = s.get
            st.lookup(name) match {
                case Some(o) => return Some(o)
                case None    => { s = st.encSymTable }
            }
        }
        None
    }

    /** Add a constant variable to the map for constant propogation */
    def addConstantVar(name: String, value: AnyVal): Unit = {
        value match {
            case n: Int     => constantIntsMap = constantIntsMap + (name -> n)
            case n: Boolean => constantBoolsMap = constantBoolsMap + (name -> n)
            case _          =>
        }

    }

    /** Given another map of constants, add it to the current map */
    def addConstants(constants: Map[String, Int]): Unit = {
        constantIntsMap = constantIntsMap ++ constants
    }

    /** To remove variables that are in the loop condition or loop body from constant map */
    def removeConstantVar(name: String): Unit = {
        var s: Option[SymbolTable] = Some(this)
        while (s != None) {
            var st = s.get
            st.constantIntsMap = constantIntsMap.removed(name)
            s = st.encSymTable

        }
    }

    /** Clears all constants from the map */
    def clearAllConstants() = {
        constantIntsMap = Map[String, Int]()
        constantBoolsMap = Map[String, Boolean]()
    }

    /** Propogates up (checks parent symbol table for constant) */
    def containsConstant(name: String): Boolean = {
        if (lookupAll(name) == None) {
            return false
        }
        val identType = lookupAll(name).get.getType()
        val constantMapType = identType match {
            case IntType()  => constantIntsMap
            case BoolType() => constantBoolsMap
            case _ => throw new RuntimeException("Identifier type does not correspond to type of constants stored")
        }

        var s: Option[SymbolTable] = Some(this)
        while (s != None) {
            var st = s.get
            if (constantMapType.contains(name)) {
                return true
            }
            s = st.encSymTable
        }
        false
    }

    /** Only called after containsConstant() check */
    def getConstant(name: String, identType: Type): AnyVal = {
        identType match {
            case IntType()  => constantIntsMap.get(name).get
            case BoolType() => constantBoolsMap.get(name).get
            case _          =>
        }

    }
}

object SymbolTable {
    def apply(): SymbolTable = new SymbolTable(None, Map[String, Identifier]())

    def apply(encSymTable: SymbolTable): SymbolTable =
        new SymbolTable(Some(encSymTable), Map[String, Identifier]())
}
