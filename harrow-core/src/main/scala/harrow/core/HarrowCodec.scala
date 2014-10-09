package harrow.core

import scala.util.parsing.input.Positional


/**
 * Harrow codec instructions
 */
trait CodecDirective

/**
 * explicitly ignore a field during initial codec
 */
case object SkipCodec extends CodecDirective

/**
 * 
 */
sealed trait ByteOrder
case object BigEndian extends ByteOrder { override def toString = "BE" } 
case object LittleEndian extends ByteOrder { override def toString = "LE" }
case class ExplicitByteOrder(byteOrder: ByteOrder) extends Positional with CodecDirective

/**
 * 
 */
case class ConditionalInput(expression: Expression) extends Positional with CodecDirective

/**
 * 
 */
sealed trait InputType extends CodecDirective

case class ExplicitValue(expr: Expression) extends InputType
case object Implied extends InputType { override def toString = "implied" }

case object Int8 extends InputType { override def toString = "int8" }
case object UInt8 extends InputType { override def toString = "uint8" }
case object Int16 extends InputType { override def toString = "int16" }
case object UInt16 extends InputType { override def toString = "uint16" }
case object Int24 extends InputType { override def toString = "int24" }
case object Int32 extends InputType { override def toString = "int32" }
case object UInt32 extends InputType { override def toString = "uint32" }
case object Int48 extends InputType { override def toString = "int48" }
case object Int64 extends InputType { override def toString = "int64" }
case object LDouble extends InputType { override def toString = "double" }

/**
 * 
 */
case class Bits(size: Int = -1, sizeExpr: Option[Expression] = None) extends InputType {
  override def toString = sizeExpr match {
    case Some(exp) => "bits:$( " + exp + " )"
    case _ => "bits:" + size
  }
}

/**
 * 
 */
case class Bytes(size: Int = -1, sizeExpr: Option[Expression], function: Option[BytesFunctionLiteral]) extends InputType {
  override def toString = sizeExpr match {
    case Some(exp) => "bytes:$( " + exp + " )"
    case _ => "bytes:" + size
  }
}

/**
 * 
 */
case class ToSize(size: Int = -1, sizeExpr: Option[Expression]) extends InputType {
  override def toString = sizeExpr match {
    case Some(exp) => "to:$( " + exp + " )"
    case _ => "to:" + size
  }
}

/**
 * 
 */
case class ContainerItems(size: Int = -1, sizeExpr: Option[Expression]) extends CodecDirective {
  override def toString = sizeExpr match {
    case Some(exp) => "items:$( " + exp + " )"
    case _ => "items:" + size
  }
}

/**
 * 
 */
case class BytesFunctionLiteral(snippet: String) extends Positional {
  override def toString = s".$snippet"
}


