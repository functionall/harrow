#Harrow
---

Harrow is a DSL for generating lazy binary decoders for complex custom binary formats.
The DSL is a thrift-like grammar for declaring codec directives and will generate:

+ Flyweight model in scala
+ Thrift IDL
+ TProtocol implementation _(read-only)_


Harrow provides an SBT plugin for code generation. This should be used in conjunction with the standard thrift plugin, scrooge or any thrift compatible code generator


## Harrow DSL
---
The DSL is effectively an extended version of thrift IDL that supports explicit codec directives.

#### Thrift Modifications

+ Adds transient type
+ Optional type inference from codec directives
+ implicit byte order header

###### Transient Type
Introduce a transient type on field declarations

```
_: transient i32 totalSize 	<- value:$(header.reportLength)
```

###### ByteOrder
header instruction for byteorder

```
[2] ByteOrder = 'implicit order =' 'big' | 'little'
```

#### Codec Directives

The codec directives provide explicit information on how to decode input from a custom binary format. The following are the main elements of the codec language

###### Codec Directive
Instructions applied to a field definition

```
[1] CodecDirective = '<-' Skip | Instruction*
```
###### Skip
When a field should be ignored.. does not exist in the raw binary

```
[2] Skip = '^^' 
```
###### Instruction
Instructions determine how input is decoded from raw binary

```
[3] Instruction = ByteOrder | InputType | ConditionalAssign | Items
```
###### ByteOrder
Explicit byte order for a read operation

```
[4] ByteOrder = 'big' | 'little'
```
###### InputType
A range of specific input types.. (see HarrowDSL.scala for all)

```
[5] InputType = 'int8' | 'uint8' | 'int16' | 'uint16' | 'int24' | 'int32' | 'uint32' | 'int48' | 'int64' | Bits | Bytes | ToSize | Value
```

###### Bits
Specific bits

```
[6] Bits = 'bits:' IntConstant | Expression
```

###### Bytes
Specific bytes which optionally supports a scala function literal.. This should be a single line implementation of a ```Function[Array[Byte], T]```

```
[7] Bytes = 'bytes:' IntConstant | Expression [ FunctionLiteral ]
```
###### FunctionLiteral
This should be a single line implementation of a ```Function[Array[Byte], T]```

```
[8] FunctionLiteral = '-> [[' ..f(bytes).. ']]'
```

###### ToSize
Greedy quantifiers

```
[8] ToSize = 'to:' IntConstant | Expression
```
###### Conditional Assignement
Conditional assignment for optional field types

```
[9] ConditionalAssign = 'if $(' Expression ')'
```

###### Expression
The DSL supports a simple expression language that can be used to bind variable and dynamic parts of complex binary formats
For details, see the harrow-examples project..

```
if $(ServiceRecord.recordHeader.recordType == 1)
```



## Harrow SBT Plugin
---
To include the Harrow SBT plugin in your project, add an explicit dependency on the harrow-sbt-plugin (source or binary where avaiable)
Then, add the following import to your SBT build

```
import harrow.sbt.HarrowSBT._
```

Add the keys to your settings, for example:

```
settings = Project.defaultSettings ++ HarrowSettings
```

Customize as required:

```
HarrowJavaPackage := "io.functionall.harrow"
```

For a full list of settings, see the HarrowSBT source.
