package harrow.core

import scala.util.parsing.input.Positional

/**
 * Context for variables
 */
sealed trait VariableReference

case class VariableSource(variable: Variable, path: List[FieldDefinition]) extends VariableReference
case class VariableProvider(variable: Variable, typeInfo: FieldDefinition, sourceContext: Struct) extends VariableReference


/**
 * 
 * Model/AST for Harrow
 * 
 * The model itself is a composition of thrift metadata and custom codec directives
 * 
 */
case class HarrowData(order: ByteOrder, headers: List[HeaderInstruction], allTypes: List[TDefinition], lazyImmutable: Boolean = true) {


  val complexDefinitions = allTypes.collect { case c: Struct => c }.map(d => (d.identifier, d)).toMap
  val availableTypes = allTypes.map(d => (d.identifier, d)).toMap

  /**
   * resolve variable references used in expressions
   * essentially this will recursively search the definition graphs and inject variable source and provider
   * instructions into type definitions to allow expressions resolve variables from anywhere that is
   * reachable within the current object graph.. or fail loudly if no such type is reachable
   */
  def resolve = {

    val _resolvedStructs = scala.collection.mutable.Map() ++ complexDefinitions
    /*
       * recursively find variables in a decode instruction
       */
    def extractVariables(instruction: CodecDirective): List[Variable] = {
      def findVars(expr: Expression, vars: List[Variable] = List.empty[Variable]): List[Variable] = {
        expr match {
          case found @ Variable(path) => found :: vars
          case UnaryOp(_, arg) => findVars(arg)
          case BinaryOp(_, left, right) => findVars(left) ::: findVars(right) ::: vars
          case IfElse(condition, left, Some(right)) => findVars(condition) ::: findVars(left) ::: findVars(right) ::: vars
          case IfElse(condition, left, None) => findVars(condition) ::: findVars(left) ::: vars
          case _ => vars
        }
      }
      instruction match {
        case ContainerItems(_, Some(expr)) => findVars(expr)
        case Bits(_, Some(expr)) => findVars(expr)
        case Bytes(_, Some(expr), _) => findVars(expr)
        case ToSize(_, Some(expr)) => findVars(expr)
        case ConditionalInput(expr) => findVars(expr)
        case ExplicitValue(expr) => findVars(expr)
        case _ => List.empty[Variable]
      }
    }

    case class ReachablePath(types: List[Struct])
    /*
       * find all possible paths from a given type to another within a composition hierarchy
       */
    def determineReach(from: Struct, to: Struct, path: List[Struct] = List.empty, all: List[ReachablePath] = List.empty): List[ReachablePath] = {
      if (from.identifier == to.identifier) {
        all :+ ReachablePath(path :+ to)
      } else {
        from.fields.flatMap(dec => {
          dec.definition.datatype match {
            case Some(DefinedType(id)) =>
              all ::: determineReach(complexDefinitions(id), to, path :+ from, all)
            case Some(SetType(DefinedType(id))) =>
              all ::: determineReach(complexDefinitions(id), to, path :+ from, all)
            case Some(ListType(DefinedType(id))) =>
              all ::: determineReach(complexDefinitions(id), to, path :+ from, all)
            case _ =>
              all
          }
        })
      }
    }

    /*
       * algorithm for resolving variables
       * - if local variable... 
       * 	+ its reachable.. add source to enclosing.. done
       * - if type reference... 
       * 	+ find the type that serves as an anchor
       *    + add source instruction to this type
       * 	+ find all paths from the anchor to the enclosing type
       *    + walk each path and add a provider instruction for the anchor
       *    .. done
       */
    complexDefinitions.values.foreach(definition => {
      val fieldNames = definition.fields.map(_.definition.identifier)
      // traverse all fields to find expression variables

      definition.fields.foreach(field => {
        val vars = field.instructions.flatMap(extractVariables _)

        vars.foreach(v => {

          val anchorName = v.path.head
          val anchorType = complexDefinitions.get(anchorName).getOrElse(definition)
          val variableSourceInfo = v.path.foldLeft((anchorType, List.empty[FieldDefinition])) { (context, path) =>
            {
              val pathField = context._1.fields.find(_.definition.identifier == path).map(_.definition)
              pathField match {
                case Some(found @ FieldDefinition(_, _, _, Some(DefinedType(id)))) =>
                  (complexDefinitions(id), context._2 :+ found)
                case Some(found @ FieldDefinition(_, _, _, Some(t: PrimitiveType))) =>
                  (context._1, context._2 :+ found)
                case None if (context._1.identifier == path && context._2.isEmpty) =>
                  context
                case otherwise =>
                  throw new HarrowException(s"Variable reference [${v.identifier}] in expression [${v.path}] cannot be resolved for decoding instructions : ${field.pos}")
              }
            }
          }

          // (type, list-fields)
          variableSourceInfo match {

            case (origin, Nil) =>
              throw new HarrowException(s"Variable reference [${v.identifier}] in expression [${v.path}] cannot be resolved for decoding instructions : ${field.pos}")

            case (origin, fields) if (origin.identifier == definition.identifier) => // local ref
              val varSource = VariableSource(v, fields)
              val updated = _resolvedStructs.get(definition.identifier).getOrElse(definition).addVariable(varSource)
              _resolvedStructs += definition.identifier -> updated

            case (origin, fields) =>
              val varSource = VariableSource(v, fields)
              val updatedSource = _resolvedStructs.get(anchorType.identifier).getOrElse(anchorType).addVariable(varSource)
              _resolvedStructs += anchorType.identifier -> updatedSource
              val varProvider = VariableProvider(v, fields.last, anchorType)
              determineReach(anchorType, definition).foreach(rp => rp.types.foreach(pathType => {
                val updatedProvider = _resolvedStructs.get(pathType.identifier).getOrElse(pathType).addVariable(varProvider)
                _resolvedStructs += pathType.identifier -> updatedProvider
              }))

          }
        })
      })
    }) // end for all definitions

    val resolvedDefinitions = availableTypes.keys.map(name => {
      _resolvedStructs.get(name).getOrElse(availableTypes(name))
    }).toList
    copy(allTypes = resolvedDefinitions)
  }

  /**
   * validates in a global scope after all types have been declared
   */
  def validate {

    // check for duplicate identifiers
    if (availableTypes.size < allTypes.size) {
      availableTypes.values.foreach(d => {
        val id = d.identifier.toLowerCase()
        if (availableTypes(id) != d)
          throw new HarrowException(s"Type [${d.identifier}] is declared twice at [${availableTypes(id).pos}] and at [${d.pos}]")
      })
    }

    // validate definitions
    complexDefinitions.values.foreach(d => {

      // check for duplicate identifiers
      val fieldDefinitions = d.fields.map(f => (f.definition.identifier.toLowerCase(), f)).toMap
      // check for duplicate field identifiers
      if (fieldDefinitions.size < d.fields.size) {
        d.fields.foreach(f => {
          val id = f.definition.identifier.toLowerCase()
          if (fieldDefinitions(id) != f) {
            throw new HarrowException(s"Field [${f.definition.identifier}] is declared twice at [${fieldDefinitions(id).pos}] and at [${f.pos}]")
          }
        })
      }

      // check for valid type references (note: implemented using value type.. would be neater to fix and do a type level implementation to abstract over the containers)
      d.fields.foreach(f => {
        def checkType(fieldType: Option[FieldType]) {
          fieldType match {
            case Some(DefinedType(id)) =>
              if (!availableTypes.contains(id))
                throw new HarrowException(s"Field [${f.definition.identifier}] : refers to non existing type [$id]  at [${f.pos}]")
            case Some(ListType(DefinedType(id))) =>
              if (!availableTypes.contains(id))
               throw new HarrowException(s"Field [${f.definition.identifier}] : refers to non existing type [$id]  at [${f.pos}]")
            case Some(SetType(DefinedType(id))) =>
              if (!availableTypes.contains(id))
                throw new HarrowException(s"Field [${f.definition.identifier}] : refers to non existing type [$id]  at [${f.pos}]")
            case Some(MapType(DefinedType(id), _)) =>
              if (!availableTypes.contains(id))
                throw new HarrowException(s"Field [${f.definition.identifier}] : refers to non existing type [$id]  at [${f.pos}]")
            case Some(MapType(_, DefinedType(id))) =>
              if (!availableTypes.contains(id))
               throw new HarrowException(s"Field [${f.definition.identifier}] : refers to non existing type [$id]  at [${f.pos}]")
            case _ =>
          }
        }
        val ftype = f.definition.datatype
        checkType(ftype)
      })

    })

  }

  def +=(other: HarrowData) = {
    copy(headers = headers ::: other.headers, allTypes = allTypes ::: other.allTypes)
  }
  

}

/**
 * 
 */
object HarrowData {
  
  /**
   *
   */
  def isValid(field: String, declared: FieldType, fsource: Positional, input: InputType, isource: Positional) {

    (declared, input) match {
      case (_, Implied) => // implied ok with any.. including only option for container types
      case (TBoolean, Bits(_, _)) => // ok.. coerce to boolean
      case (TByte, Bits(_, _)) => // ok
      case (TByte, Int8) => // ok
      case (TByte, Bytes(1, _, _)) => // ok
      case (TI16, Bits(_, _)) => // ok
      case (TI16, Int8) => // ok
      case (TI16, UInt8) => // ok
      case (TI16, Int16) => // ok
      case (TI32, Int8) => // ok
      case (TI32, UInt8) => // ok
      case (TI32, Int16) => // ok
      case (TI32, Int24) => // ok
      case (TI32, UInt16) => // ok
      case (TI32, Int32) => // ok
      case (TI64, Int8) => // ok
      case (TI64, UInt8) => // ok
      case (TI64, Int16) => // ok
      case (TI64, Int24) => // ok
      case (TI64, UInt16) => // ok
      case (TI64, Int32) => // ok
      case (TI64, Int48) => // ok
      case (TI64, UInt32) => // ok
      case (TI64, Int64) => // ok
      case (TDouble, LDouble) => // ok
      case (TDouble, Bytes(_, _, _)) => // ok 
      case (TString, _) => // ok
      case (ListType(_), ToSize(_, _)) => // ok 
      case (ListType(_), Bytes(_, _, _)) => // ok 
      case (SetType(_), ToSize(_, _)) => // ok 
      case (SetType(_), Bytes(_, _, _)) => // ok 
      case (MapType(_,_), ToSize(_, _)) => // ok 
      case (_, ExplicitValue(_)) => // ok
      case (ftype, itype) => 
        throw new HarrowException(s"Field [$field] with declared type '$ftype' at [${fsource.pos}] is not compatible with decoding instruction of type '$itype' at [${isource.pos}]")
    }

  }

  /**
   * transformation that infers types in a bi-directional fashion between a field declaration and
   * a set of decoding instructions
   */
  def inferTypes(field: FieldDeclaration): FieldDeclaration = {

    val explicitInputType = field.instructions.collect { case i: InputType => i }.headOption

    def deriveInputType(ftype: Option[FieldType]): InputType = {
      ftype match {
	      case Some(TBoolean) => Bits(1)
	      case Some(TByte) => Bytes(1, None, None)
	      case Some(TI16) => Int16
	      case Some(TI32) => Int32
	      case Some(TI64) => Int64
	      case Some(TDouble) => LDouble
	      case Some(ListType(nested: PrimitiveType)) => deriveInputType(Some(nested))
	      case Some(SetType(nested: PrimitiveType)) => deriveInputType(Some(nested))
	      case Some(nested: ContainerType) => Implied
	      case _ => throw new HarrowException(s"Cannot infer codec instructions for field [$field] at [${field.pos}]")
      }
    }
    
    def derivedInputType = deriveInputType(field.definition.datatype)
   
    def derivedFieldType: FieldType = explicitInputType match {
      case Some(Bits(1, _)) => TBoolean
      case Some(Int8) => TByte
      case Some(UInt8) => TI16
      case Some(Int16) => TI16
      case Some(Int24) => TI32
      case Some(UInt16) => TI32
      case Some(Int32) => TI32
      case Some(Int48) => TI64
      case Some(UInt32) => TI64
      case Some(Int64) => TI64
      case Some(LDouble) => TDouble
      case Some(Bytes(_, _, _)) => TBinary
      case other => throw new HarrowException(s"Cannot infer codec instructions for field [$field] at [${field.pos}]")
    }

    val declarationType = field.definition.datatype.getOrElse(derivedFieldType)
    val inputType = explicitInputType.getOrElse(derivedInputType)
    // check valid
    isValid(field.definition.identifier, declarationType, field, inputType, field)

    val refinedInstructions = inputType :: field.instructions.filterNot(_.isInstanceOf[InputType])

    field.usingDefinition(field.definition.withType(declarationType)).usingInstructions(refinedInstructions)

  }
  
}