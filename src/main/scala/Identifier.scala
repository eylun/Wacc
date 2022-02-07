/* Identifier Objects */

sealed trait Identifier

sealed trait Type extends Identifier

case class Variable(t: Type) extends Identifier

case class Param(t: Type) extends Identifier

/* Basic types */
case class IntType(min: Int, max: Int) extends Type

case class BoolType() extends Type

case class CharType() extends Type

case class StringType() extends Type

case class NestedPairType() extends Type

case class ArrayType(elemType: Type, var elements: Int) extends Type

case class PairType(fstType: Type, sndType: Type) extends Type

case class NullPairType() extends Type

case class FunctionId(returnType: Type, params: Array[Param], 
                      symbolTable: SymbolTable) extends Identifier
