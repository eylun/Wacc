import scala.collection.mutable.Map

class SymbolTable(
    val encSymTable: Option[SymbolTable],
    dict: Map[String, Identifier]
) {

    /** Adds an identifier string to the current symbol table, mapped to an
      * identifier object.
      */
    def add(name: String, obj: Identifier): Unit = {
        dict.addOne(name -> obj)
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
