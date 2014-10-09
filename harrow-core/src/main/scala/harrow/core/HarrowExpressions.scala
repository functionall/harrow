package harrow.core

import com.dongxiguo.fastring.Fastring.Implicits._

/**
 * Abstraction for simple expression in the DSL
 */
sealed trait Expression

/**
 * 
 */
case class LiteralValue(value: String) extends Expression { override def toString = value }

/**
 * 
 */
case class Variable(pathSpec: String) extends Expression {
  
  lazy val pathElements = pathSpec.split("\\.").toList
  
  lazy val accessor = {
    val inCaps = pathElements.map(_.capitalize)
    fast"get${inCaps.mkString("")}Var".toString()
  }
  
  lazy val identifier = pathElements.last
  
  lazy val path = pathElements
  
  override def toString = fast"this.$accessor()".toString
  
}

object Variable {
  def apply(path: List[String]) = new Variable(path.mkString("."))
}


/**
 * 
 */
sealed trait Operator

case object Plus extends Operator { override def toString = "+" }
case object Minus extends Operator { override def toString = "-" }
case object Multiply extends Operator { override def toString = "*" }
case object Divide extends Operator { override def toString = "/" }
case object Mod extends Operator { override def toString = "%" }
case object Equals extends Operator { override def toString = "==" }
case object GreaterThan extends Operator { override def toString = ">" }
case object LessThan extends Operator { override def toString = "<" }
case object Assign extends Operator { override def toString = "=" }

/**
 * 
 */
case class UnaryOp(operator: Operator, arg: Expression) extends Expression {
  override def toString = fast"$operator $arg".toString
}

/**
 * 
 */
case class BinaryOp(operator: Operator, left: Expression, right: Expression) extends Expression {
  override def toString = fast"$left $operator $right".toString
}

/**
 * 
 */
case class Or(conditions: Expression*) extends Expression {
  override def toString = conditions.mkString("(", ") || (", ")")
}

/**
 * 
 */
case class IfElse(condition: Expression, left: Expression, right: Option[Expression]) extends Expression {
  
  override def toString = {
    val ifPart = fast" if ($condition) { \n\t\t$left \n\t\t} ".toString
    right match {
      case Some(exp) => fast"($ifPart else { \n\t\t$exp \n\t\t})".toString
      case _ => ifPart
    }
  }
  
}