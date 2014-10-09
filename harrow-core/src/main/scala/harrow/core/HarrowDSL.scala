package harrow.core

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.input.Position

/**
 * Harrow DSL
 * 
 * The DSL is a Thrift IDL - like language that ::
 * - allows forward references which enables top-down/sequential definitions
 * - takes codec directives in the form of <- ..directive..
 * - supports expressions and conditional assignment
 * - type inference from codec directives
 * - implicit codec directives from type metadata
 * - supports transient types for encoding metadata
 * - validates type compatibility and some level of casting and lifting of types
 * 
 */
trait HarrowDSL extends JavaTokenParsers {

  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  
  import HarrowData._
  
  /*
   * bastardized thrift
   */

  def dataModel: Parser[HarrowData] = 
    opt(byteOrder) ~ rep(headers) ~ rep(definition) ^^ {
	    case order ~ headers ~ defs =>
	      val bOrder = order.getOrElse(BigEndian)
	      val fixed = defs.map(d => {
	        var _idx = 0
	        d match {
	          case c: Struct =>
	            val typedFields = c.fields.map(f => {
	              val fx = inferTypes(f)
	              fx.definition.numericId match {
	                case Some(id) =>
	                  _idx = id
	                  fx
	                case None if (fx.definition.scope != IsTransient) =>
	                  _idx += 1
	                  val newDef = fx.definition.copy(numericId = Some(_idx))
	                  fx.copy(definition = newDef)
	                case _ => fx
	              }
	            })
	            c.withFields(typedFields)
	          case s => s
	        }
	      })
      HarrowData(bOrder, headers, fixed)
   }
  
  def definition: Parser[TDefinition] =
    const | typedef | enum | struct | union

  def headers: Parser[HeaderInstruction] =
    include | importHeader | namespace

  def byteOrder: Parser[ByteOrder] =
    ("implicit" ~> "order" ~> "=" ~> endianess)

  def endianess: Parser[ByteOrder] =
    ("big" | "little") ^^ (order => order match {
      case "little" => LittleEndian
      case _ => BigEndian
    })

  def include: Parser[Include] =
    "include" ~> literal ^^ (file => Include(file.replace("\"", "")))

  def typeName: Parser[String] =
    """[\p{Upper}\p{javaJavaIdentifierPart}]*""".r
    
  def identifier: Parser[String] =
    """[\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}]*""".r

  def qualifiedIdentifier: Parser[String] =
    """[\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}[\\.]]*""".r

  def importHeader: Parser[Import] =
    "import" ~> qualifiedIdentifier ^^ (name => Import(name))
    
  def namespace: Parser[Namespace] =
    "namespace" ~> qualifiedIdentifier ^^ (name => Namespace(name))

  def listSeparator = "," | ";"

  def constValue: Parser[String] =
    (numericString | literal | qualifiedIdentifier | hexadecimalString | constList | constMap)

  def constList: Parser[String] =
    "[" ~> rep(constValue <~ opt(listSeparator)) <~ "]" ^^ {
      case values => values.mkString("[", ",", "]")
    }

  def constMap: Parser[String] =
    ("{" ~> rep((constValue <~ ":") ~ constValue <~ opt(listSeparator)) <~ "}") ^^ {
      case values => values.map(kv => s"""${kv._1} : ${kv._2}""").mkString("{", ",", "}")
    }

  def numericString: Parser[String] =
    (intConstant | doubleConstant) ^^ (_.toString)

  def hexadecimalString: Parser[String] =
    """0x\p{XDigit}*""".r

  def intConstant: Parser[Int] =
    wholeNumber ^^ (_.toInt)

  def doubleConstant: Parser[Double] =
    decimalNumber ^^ (_.toDouble)

  def literal: Parser[String] =
    stringLiteral

  def booleanLiteral: Parser[Boolean] =
    ("true" | "false") ^^ (_.toBoolean)

  def fieldID: Parser[Int] =
    (intConstant | "_") <~ ":" ^^ {
      case "_" => 0
      case value: Int => value
    }

  def fieldReq: Parser[FieldScope] =
    ("required" | "optional" | "transient") ^^ {
      case "required" => IsRequired
      case "optional" => IsOptional
      case _ => IsTransient
    }

  def primitiveType: Parser[FieldType] =
    ("bool" | "byte" | "i16" | "i32" | "i64" | "double" | "binary" | "string") ^^ {
      case "bool" => TBoolean
      case "byte" => TByte
      case "i16" => TI16
      case "i32" => TI32
      case "i64" => TI64
      case "double" => TDouble
      case "binary" => TBinary
      case _ => TString
    }
  
  def complexType: Parser[DefinedType] = 
    typeName ^^ { DefinedType(_) }

  def fieldType: Parser[FieldType] =
    (primitiveType | typeName) ^^ {
      case x: FieldType => x
      case deftype: String => DefinedType(deftype)
    }

  def definitionType: Parser[FieldType] =
    (primitiveType | containerType | complexType)

  def containerType: Parser[FieldType] =
   (listType | setType | mapType)

  def mapType: Parser[MapType[_, _]] =
    ("map" ~> keyType ~ valueType) ^^ {
      case ktpe ~ vtpe => MapType(ktpe, vtpe)
    }

  def keyType: Parser[FieldType] =
    "<" ~> fieldType <~ ","

  def valueType: Parser[FieldType] =
    fieldType <~ ">"

  def setType: Parser[FieldType] =
    "set" ~> ("<" ~> fieldType <~ ">") ^^ {
      case tpe => SetType(tpe)
    }

  def listType: Parser[FieldType] =
    "list" ~> ("<" ~> fieldType <~ ">") ^^ {
      case tpe => ListType(tpe)
    }

  def const: Parser[Const] =
    "const" ~> fieldType ~ identifier ~ ("=" ~> constValue <~ opt(listSeparator)) ^^ {
      case typ ~ name ~ value => Const(name, typ, value)
    }

  def typedef: Parser[TypeDefAlias] =
    "typedef" ~> definitionType ~ identifier ^^ {
      case typ ~ name => TypeDefAlias(name, typ)
    }

  def enum: Parser[Enum] =
    "enum" ~> identifier ~ ("{" ~> rep(enumValue) <~ "}") ^^ {
      case name ~ values => Enum(name, values)
    }

  def enumValue: Parser[(String, Int)] =
    identifier ~ "=" ~ (intConstant | hexadecimalString) <~ opt(listSeparator) ^^ {
      case name ~ eq ~ value => value match {
        case i: Int => (name, i)
        case h: String => (name, Integer.parseInt(h, 16))
      } 
    }

  def struct: Parser[Struct] =
    "struct" ~> identifier ~ ("{" ~> rep(field) <~ "}") ^^ {
      case name ~ fields => Struct(name, fields, List.empty[VariableReference])
    }

  def union: Parser[Struct] =
    "union" ~> identifier ~ ("{" ~> rep(field) <~ "}") ^^ {
      case name ~ fields => Struct(name, fields, List.empty[VariableReference], true)
    }

  def field: Parser[FieldDeclaration] =
    opt(fieldID) ~ opt(fieldReq) ~ opt(definitionType) ~ identifier ~ opt("=" ~> constValue) ~ opt(listSeparator) ~ decodeInstructions ^^ {
      case numericId ~ explicitScope ~ typ ~ identifier ~ defvalue ~ sep ~ instructions =>
        val scope = explicitScope.getOrElse(IsOptional)
        FieldDeclaration(FieldDefinition(identifier, numericId, scope, typ), instructions)
    }

  /*
   * codec instructions
   */

  def decodeInstructions: Parser[List[CodecDirective]] =
    mapper ~> (skip | instructions)
    
  def instructions: Parser[List[CodecDirective]] =
    opt(endianess) ~ opt(inputType) ~ opt(conditionalAssignment) ~ opt(items) ^^ {
      case order ~ input ~ cond ~ items =>
        val instructions = order.map(o => ExplicitByteOrder(o)) :: cond :: input :: items :: Nil
        instructions.flatten
    }

  def mapper = "<-"

  def skip: Parser[List[CodecDirective]] = "^^" ^^ { _ => List(SkipCodec) }

  def bits: Parser[Bits] =
    "bits:" ~> size ^^ {
      case Left(size) => Bits(size, None)
      case Right(sizeExpr) => Bits(0, Some(sizeExpr))
    } 

  def inputType: Parser[InputType] =
    ("int8" | "uint8" | "int16" | "uint16" | "int24" | "int32" | "uint32" | "int48" | "int64" | bits | bytes | toSize | value) ^^ {
      case "int8" => Int8
      case "uint8" => UInt8
      case "int16" => Int16
      case "uint16" => UInt16
      case "int24" => Int24
      case "int32" => Int32
      case "uint32" => UInt32
      case "int48" => Int48
      case "int64" => Int64
      case vartype: InputType => vartype
    }

  // basic expressions

  def plus: Parser[Operator] = "+" ^^ (_ => Plus)
  def minus: Parser[Operator] = "-" ^^ (_ => Minus)
  def multiply: Parser[Operator] = "*" ^^ (_ => Multiply)
  def divide: Parser[Operator] = "/" ^^ (_ => Divide)
  def mod: Parser[Operator] = "%" ^^ (_ => Mod)
  def equals: Parser[Operator] = "==" ^^ (_ => Equals)
  def gt: Parser[Operator] = ">" ^^ (_ => GreaterThan)
  def lt: Parser[Operator] = "<" ^^ (_ => LessThan)
  def assign: Parser[Operator] = "+" ^^ (_ => Assign)
  def operator = (plus | minus | multiply | divide | mod | gt | lt | equals)

  def variable: Parser[Variable] =
    opt("(") ~> qualifiedIdentifier <~ opt(")")  ^^ (Variable(_)) 

  def term: Parser[Expression] =
    opt("(") ~> (hexadecimalString | intConstant | doubleConstant | booleanLiteral | literal | variable) <~ opt(")") ^^ {
      case i: Int => LiteralValue(i.toString)
      case d: Double => LiteralValue(d.toString)
      case b: Boolean => LiteralValue(b.toString)
      case s: String => LiteralValue(s)
      case v: Variable => v
    }

  def unaryOp: Parser[UnaryOp] =
    opt("(") ~> operator ~ term <~ opt(")") ^^ {
      case op ~ expr => UnaryOp(op, expr)
    }

  def valueConditional: Parser[Variable] = {
    opt("(") ~> variable <~ opt(")") ^^ {
      case v => v
    }
  }

  def binaryOp: Parser[BinaryOp] =
    opt("(") ~> term ~ operator ~ statement <~ opt(")") ^^ {
      case expr1 ~ op ~ expr2 => BinaryOp(op, expr1, expr2)
    }

  def conditional: Parser[Expression] =
    (binaryOp | valueConditional) 

  def orConditional: Parser[Or] =
    "(" ~> conditional ~ rep("||" ~> conditional) <~ opt(")") ^^ {
      case first ~ others => Or((first :: others): _*)
    }

  def composedOps: Parser[Expression] =
    binaryOp ~ rep(unaryOp) ^^ {
      case start ~ rest => rest.foldLeft(start)((left, right) => BinaryOp(right.operator, left, right.arg))
    }

  def expression: Parser[Expression] =
    (composedOps | binaryOp | unaryOp | term)

  def block: Parser[Expression] =
    "{" ~> expression <~ "}"

  def conditionalBlock: Parser[IfElse] =
    ("if" ~> (conditional | orConditional) ~ block ~ opt("else" ~> block)) ^^ {
      case cond ~ left ~ right => IfElse(cond, left, right)
    }

  def statement: Parser[Expression] =
    opt("(") ~> (conditionalBlock | composedOps | binaryOp | term) <~ opt(")")

  def conditionalAssignment: Parser[ConditionalInput] =
    "if" ~ "$(" ~> opt("(") ~> (orConditional | statement) <~ opt(")") <~ opt(")") ^^ {
      case expr => ConditionalInput(expr)
    }

  def explicitAssignment: Parser[Expression] =
    "$(" ~> statement <~ opt(")") ^^ {
      case expr => expr
    }

  def arbitraryStingBeforeBrackets = """.+?(?=(?:\]\]))""".r

  def mapWithFunc: Parser[String] =
    "~>" ~ "[[" ~> arbitraryStingBeforeBrackets <~ "]]"

  def size: Parser[Either[Int, Expression]] =
    (intConstant | ("$(" ~> statement <~ opt(")"))) ^^ {
      case i: Int => Left(i)
      case e: Expression => Right(e)
    }

  def bytes: Parser[Bytes] =
    "bytes:" ~> size ~ opt(mapWithFunc) ^^ {
      case Left(size) ~ func => Bytes(size, None, func.map(BytesFunctionLiteral(_)))
      case Right(sizeExpr) ~ func => Bytes(0, Some(sizeExpr), func.map(BytesFunctionLiteral(_)))
    }
  
  def toSize: Parser[ToSize] =
    "to:" ~> size ^^ {
      case Left(size) => ToSize(size, None)
      case Right(sizeExpr) => ToSize(0, Some(sizeExpr))
  }

  def value: Parser[ExplicitValue] =
    "value:" ~> (explicitAssignment | term) ^^ {
      case complex: Expression => ExplicitValue(complex)
    }

  def items: Parser[ContainerItems] =
    "items:" ~> (intConstant | explicitAssignment | term) ^^ {
      case i: Int => ContainerItems(i, None)
      case complex: Expression => ContainerItems(-1, Some(complex))
    }

}


/**
 *
 */
object HarrowDSLTest extends HarrowDSL {
  
  def main(args: Array[String]) {
    val result = parseAll(toSize, "to:$(header.reportLength - (if (header.msgFlags.extensionFlag) { header.extensionHeader.exHeadLength + 2 } else { 0 }) )")
    println(s"Parsed $result")
    println()
    def doPrint(exp: Expression): Unit = {
      exp match {
          case found @ Variable(path) => println(s"   VAR $path")
          case UnaryOp(op, arg) => println(s"Unary $op ${doPrint(arg)}")
          case BinaryOp(op, left, right) => println(s"Binary $op ${doPrint(left)} ${doPrint(right)}")
          case IfElse(condition, left, Some(right)) => println(s"IfElse ${doPrint(condition)} ${doPrint(left)} ${doPrint(right)}")
          case IfElse(condition, left, None) => println(s"IfElse ${doPrint(condition)} ${doPrint(left)}")
          case other => println(s"unmatched :: ${other.getClass()} ${other}") 
      }
    }
    result.get.sizeExpr.foreach(doPrint)
  }
  
}
