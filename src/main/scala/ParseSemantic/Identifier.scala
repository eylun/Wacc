/* Identifier Objects */

/** Identifier trait for all other possible Identifiers */
sealed trait Identifier {

    /** getType() will always return only a type
      *
      * No Variables, No Params, No FunctionIDs
      */
    def getType(): Type
}

/** Type trait for all identifiers which are types */
sealed trait Type extends Identifier {
    override def getType(): Type = this
}

/** Variable - newly declared identifiers */
case class Variable(t: Type) extends Identifier {
    override def getType(): Type = t
}

/** Param - variables that are declared in function parameters */
case class Param(t: Type) extends Identifier {
    override def getType(): Type = t
    override def toString(): String = t.toString()
}

/** Any - special type to capture empty lists */
case class AnyType() extends Type {
    override def toString(): String = "ANY"
}

/* Basic types */
case class IntType() extends Type {
    override def toString(): String = "INT"
}

case class BoolType() extends Type {
    override def toString(): String = "BOOL"
}

case class CharType() extends Type {
    override def toString(): String = "CHAR"
}

case class StringType() extends Type {
    override def toString(): String = "STRING"
}

/** Nested Pair - pairs that are inside pairs (E.g. Pair(Pair, Pair)) */
case class NestedPairType() extends Type {
    override def toString(): String = "PAIR"
}

/** Array - array types store the type of its elements and its dimension
  *
  * 2D arrays have a dimension of 2, 3D arrays have a dimension of 3, etc...
  */
case class ArrayType(elemType: Type, length: List[Int], var dimension: Int)
    extends Type {
    override def toString(): String = elemType.toString() + "[]" * dimension
}

object ArrayType {
    def apply(elemType: Type, dimension: Int): ArrayType =
        ArrayType(elemType, List(0), dimension)
}

/** Pair - Pair type that contains 2 types within */
case class PairType(fstType: Type, sndType: Type) extends Type {
    override def toString(): String = s"PAIR(${fstType}, ${sndType})"
}

/** Null Pair - Pair type that contains nothing */
case class NullPairType() extends Type {
    override def toString(): String = "PAIR"
}

/** Function - Function with return type, params and its own symbol table */
case class FunctionId(
    returnType: Type,
    params: Array[Param],
    symbolTable: SymbolTable
) extends Identifier {
    override def getType(): Type = returnType
}
