import scala.collection.immutable.Map

class SymbolTable(
    var encSymTable: Option[SymbolTable],
    var dict: Map[String, Identifier]
) {
    var order: List[String] = List.empty

    /** Sets the parent of this symbol table. */
    def setParent(parent: SymbolTable): Unit = encSymTable = Some(parent)

    /** Adds an identifier string to the current symbol table, mapped to an
      * identifier object.
      */
    def add(name: String, obj: Identifier): Unit = {
        dict = dict + (name -> obj)
        order ++= List(name)
    }

    /** Looks up an identifier string in the current symbol table. Returns a
      * Some() object containing the identifier object if it exists, otherwise
      * returns None.
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
}

object SymbolTable {
    def apply(): SymbolTable = new SymbolTable(None, Map[String, Identifier]())

    def apply(encSymTable: SymbolTable): SymbolTable =
        new SymbolTable(Some(encSymTable), Map[String, Identifier]())
}
