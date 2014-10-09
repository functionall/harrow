package harrow.core

import com.dongxiguo.fastring.Fastring.Implicits._

/**
 * Returns default values for a given type when a path member is optional and not resolved
 */
object HarrowDefaults {
  
  /**
   * 
   */
  def any(typeInfo: FieldType) = {
    typeInfo match {
      case p: PrimitiveType => value(p)
      case c: ContainerType => empty(c)
    }
  }
  
  /**
   * 
   */
  def value(typeInfo: PrimitiveType) = {
    typeInfo match { 
          case TBoolean => " false "
          case TString => " \"\" "
          case TBinary => " Array.empty[Byte] "
          case _ => " 0 " // numeric
    }
  }
  
  /**
   * 
   */
  def empty(typeInfo: ContainerType) = {
    typeInfo match {
      case ListType(vtype) => fast" List.empty[$vtype] ".toString
      case SetType(vtype) => fast" Set.empty[$vtype] ".toString
      case MapType(ktype, vtype) => fast" Map.empty[$ktype, $vtype] ".toString
      case _ => " null "
    }
  }
  
}