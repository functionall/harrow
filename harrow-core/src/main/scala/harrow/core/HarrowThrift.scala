package harrow.core

import com.dongxiguo.fastring.Fastring.Implicits._
import scala.util.parsing.input.Positional


/**
 * 
 */
sealed trait HeaderInstruction

case class Import(path: String) extends HeaderInstruction
case class Include(path: String) extends HeaderInstruction
case class Namespace(path: String) extends HeaderInstruction

/**
 * 
 */
sealed trait TDefinition extends Positional {
  def identifier: String
}

/**
 * 
 */
case class Struct(identifier: String, fields: List[FieldDeclaration], variables: List[VariableReference], isUnion: Boolean = false) extends TDefinition {
  
  def addVariable(_var: VariableReference) = copy(variables = variables :+ _var)
  def withVariables(_vars: List[VariableReference]) = copy(variables = _vars)
  def extendWith(_field: FieldDeclaration) = copy(fields = fields :+ _field)
  def withFields(_fields: List[FieldDeclaration]): Struct = copy(fields = _fields)
  
}

case class Enum(identifier: String, fields: List[(String, Int)]) extends TDefinition
case class Const(identifier: String, typeInfo: FieldType, value: String) extends TDefinition
case class TypeDefAlias(identifier: String, targetType: FieldType) extends TDefinition

/**
 *
 */
case class FieldDefinition(identifier: String, numericId: Option[Int] = None, scope: FieldScope = IsOptional, datatype: Option[FieldType] = None) {

  def withType(ftype: FieldType) = copy(datatype = Some(ftype))
  def withNumericId(id: Int) = copy(numericId = Some(id))

}
/**
 * 
 */
case class FieldDeclaration(definition: FieldDefinition, instructions: List[CodecDirective]) extends Positional {

  def processWith(instruction: CodecDirective) = {
    (definition, instruction) match {
      case (FieldDefinition(_, _, IsRequired, _), SkipCodec) =>
        throw new HarrowException(s"Cannot ignore coded instructions for required field [${definition.identifier}] at [${pos}]")
      case (FieldDefinition(_, _, IsRequired, _), ConditionalInput(_)) =>
        throw new HarrowException(s"Cannot using conditional assignment instructions on a required field [${definition.identifier}] at [${pos}]")
      case (_, ins: InputType) =>
        val alreadyHasInput = instructions.collect { case i: InputType => i }
        alreadyHasInput.map(found => 
          throw new HarrowException(s"Detected multiple typed codec instructions on field [${definition.identifier}] at [${pos}]"))
      case _ =>
    }
    copy(instructions = instruction :: instructions)
  }

  def usingDefinition(fieldDef: FieldDefinition) = copy(definition = fieldDef)
  def usingInstructions(decodeInstructions: List[CodecDirective]) = copy(instructions = decodeInstructions)

}

/**
 * supported scopes
 */
sealed trait FieldScope

case object IsRequired extends FieldScope { override def toString = "required" }
case object IsOptional extends FieldScope { override def toString = "optional" }
case object IsTransient extends FieldScope { override def toString = "transient"}

/**
 * supported types
 */
sealed trait FieldType extends Positional

trait PrimitiveType extends FieldType { def toBoxed: String }
trait ContainerType extends FieldType

case object TBoolean extends PrimitiveType {
  override def toString = "bool"
  override def toBoxed = "Boolean"
}
case object TByte extends PrimitiveType {
  override def toString = "byte"
  override def toBoxed = "Byte"
}
case object TI16 extends PrimitiveType {
  override def toString = "i16"
  override def toBoxed = "Short"
}
case object TI32 extends PrimitiveType {
  override def toString = "i32"
  override def toBoxed = "Int"
}
case object TI64 extends PrimitiveType {
  override def toString = "i64"
  override def toBoxed = "Long"
}
case object TDouble extends PrimitiveType {
  override def toString = "double"
  override def toBoxed = "Double"
}
case object TBinary extends PrimitiveType {
  override def toString = "binary"
  override def toBoxed = "List[Byte]"
}
case object TString extends PrimitiveType {
  override def toString = "string"
  override def toBoxed = "String"
}

case class ListType[T <: FieldType](valueType: T) extends ContainerType {
  override def toString = fast"list<$valueType>".toString
}
case class SetType[T <: FieldType](valueType: T) extends ContainerType {
  override def toString = fast"set<$valueType>".toString
}
case class MapType[K <: FieldType, V <: FieldType](keyType: K, valueType: V) extends ContainerType {
  override def toString = fast"map<$keyType,$valueType>".toString
}

case class DefinedType(identifier: String) extends ContainerType {
  override def toString = identifier
}


