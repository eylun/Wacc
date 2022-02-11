/* Identifier Objects */

sealed trait Identifier {
    def getType(): Type
}

sealed trait Type extends Identifier {
    override def getType(): Type = identity(this)
}

case class Variable(t: Type) extends Identifier {
    override def getType(): Type = t
}

case class Param(t: Type) extends Identifier {
    override def getType(): Type = t
    override def toString(): String = t.toString()
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

case class NestedPairType() extends Type {
    override def toString(): String = "PAIR"
}

case class AnyType() extends Type

case class ArrayType(elemType: Type, var dimension: Int) extends Type {
    override def toString(): String = elemType.toString() + "[]" * dimension
}

case class PairType(fstType: Type, sndType: Type) extends Type {
    override def toString(): String = s"PAIR(${fstType}, ${sndType})"
}

case class NullPairType() extends Type {
    override def toString(): String = "PAIR"
}

case class FunctionId(
    returnType: Type,
    params: Array[Param],
    symbolTable: SymbolTable
) extends Identifier {
    override def getType(): Type = returnType
}
