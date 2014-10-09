package harrow.core

import org.apache.thrift._
import org.apache.thrift.protocol._
import java.nio.ByteBuffer


/**
 * TProtocol implementation over custom binary formats..
 * Implements a simple state machine of AsTProtocolData types to decode custom binary formats natively in thrift
 */
class TProtocolBinder(initial: AsTProtocolData) extends TProtocolReadOnlyTemplate {

  private var _readState: AsTProtocolData = new TProtocolStart(initial)
  private def transition {
    _readState = _readState.nextRead
  }

  override def readStructBegin: TStruct = {
    transition
    _readState.readStructBegin
  }
  override def readFieldBegin: TField = {
    transition
    _readState.readFieldBegin
  }

  override def readBool: Boolean = _readState.readBool
  override def readByte: Byte = _readState.readByte
  override def readI16: Short = _readState.readI16
  override def readI32: Int = _readState.readI32
  override def readI64: Long = _readState.readI64
  override def readDouble: Double = _readState.readDouble
  override def readString = _readState.readString
  override def readBinary: ByteBuffer = _readState.readBinary
  
  override def readListBegin: TList = _readState.readListBegin
  override def readSetBegin: TSet = _readState.readSetBegin
  override def readMapBegin: TMap = _readState.readMapBegin

}


/**
 * TProtocol read op abstraction that supports chaining
 */
trait AsTProtocolData extends TProtocolReadOps {
  def nextRead: AsTProtocolData
}

/**
 * 
 */
class TProtocolStop(next: AsTProtocolData) extends AsTProtocolData {
  override def readFieldBegin = new TField("stop", TType.STOP, 0)
  def nextRead = next
}

/**
 *
 */
object TProtocolEOF extends AsTProtocolData {
  override def readFieldBegin = new TField("stop", TType.STOP, 0)
  def nextRead = throw new HarrowException("Generated TProtocol error.. no more transitions available..")
}

/**
 * 
 */
class TProtocolStart(next: AsTProtocolData) extends AsTProtocolData {
  def nextRead = next
}


/**
 * Template for a read-only TProtocol implementation
 */
class TProtocolReadOnlyTemplate extends TProtocol(null) with TProtocolReadOps with TProtocolDisabledWriteOps

/**
 * TProtcol read ops template
 */
trait TProtocolReadOps {

  /** read functions */

  def readBool: Boolean = throw new HarrowException("protocol error :: not implemented for current read state")
  def readByte: Byte = throw new HarrowException("protocol error :: not implemented for current read state")
  def readI16: Short = throw new HarrowException("protocol error :: not implemented for current read state")
  def readI32: Int = throw new HarrowException("protocol error :: not implemented for current read state")
  def readI64: Long = throw new HarrowException("protocol error :: not implemented for current read state")
  def readDouble: Double = throw new HarrowException("protocol error :: not implemented for current read state")
  def readString: String = throw new HarrowException("protocol error :: not implemented for current read state")
  def readBinary: ByteBuffer = throw new HarrowException("protocol error :: not implemented for current read state")
  
  def readMessageBegin(): TMessage = 
    throw new HarrowException("protocol error :: not implemented for current read state")
  def readMessageEnd {}

  def readStructBegin(): TStruct = 
    throw new HarrowException("protocol error :: not implemented for current read state")
  def readStructEnd() {}
  def readFieldBegin(): TField = 
    throw new HarrowException("protocol error :: not implemented for current read state")
  def readFieldEnd() {}

  def readMapBegin(): TMap = 
    throw new HarrowException("protocol error :: not implemented for current read state")
  def readMapEnd() {}
  def readListBegin(): TList = 
    throw new HarrowException("protocol error :: not implemented for current read state")
  def readListEnd() {}
  def readSetBegin(): TSet = 
    throw new HarrowException("protocol error :: not implemented for current read state")
  def readSetEnd() {}

}

/**
 * Disabled TProtocol write ops template
 */
trait TProtocolDisabledWriteOps {

  /** disabled write functions  */

  def writeBool(b: Boolean) = { }
  def writeByte(b: Byte) = {  }
  def writeI16(i16: Short) = { }
  def writeI32(i32: Int) = { }
  def writeI64(i64: Long) = { }
  def writeDouble(double: Double) = { }
  def writeString(str: String) = { }
  def writeBinary(buffer: ByteBuffer) = { }
  
  def writeMessageBegin(message: TMessage) = 
    throw new HarrowException("protocol error :: this is a read-only custom TProtocol implementation")
  def writeMessageEnd() = {}
  
  def writeStructBegin(struct: TStruct) = 
    throw new HarrowException("protocol error :: this is a read-only custom TProtocol implementation")
  def writeStructEnd() = { }
  def writeFieldBegin(field: TField) = { }
  def writeFieldEnd() = { }
  def writeFieldStop() = { }
  
  def writeMapBegin(map: TMap) = { }
  def writeMapEnd() = { }
  def writeListBegin(list: TList) = { }
  def writeListEnd() = { }
  def writeSetBegin(set: TSet) = { }
  def writeSetEnd() = { }
  
}