package harrow.core

/**
 * Decoding ops over a bytebuffer 
 */
class ValuesDecoder {

  private var _buff = Array.empty[Byte]

  /**
   *
   */
  def bind(buffer: Array[Byte]) {
    _buff = buffer
  }

  /**
   *
   */
  def isBitSet(offset: Int, bitPosition: Int): Boolean = {
    (_buff(offset) & (0x1 << bitPosition)) != 0
  }
  
  /**
   *
   */
  def unpackBits(offset: Int, numBits: Int, bitPosition: Int = 0): Short = {
     numBits match {
      case 1 => (((_buff(offset & 0xFF) >>> bitPosition)) & 0x1).toShort 
      case 2 => (((_buff(offset & 0xFF) >>> bitPosition)) & 0x3).toShort 
      case 3 => (((_buff(offset & 0xFF) >>> bitPosition)) & 0x7).toShort 
      case 4 => (((_buff(offset & 0xFF) >>> bitPosition)) & 0xF).toShort
      case 5 => (((_buff(offset & 0xFF) >>> bitPosition)) & 0x1F).toShort 
      case 6 => (((_buff(offset & 0xFF) >>> bitPosition)) & 0x3F).toShort 
      case 7 => (((_buff(offset & 0xFF) >>> bitPosition)) & 0x7F).toShort 
    }
  }
  
  /**
   *
   */
  def getByte(offset: Int): Byte = {
    _buff(offset)
  }
  
  /**
   *
   */
  def getInt8(offset: Int): Short  = {
    (_buff(offset) & 0xFF).toShort
  }

  /**
   *
   */
  def getUByte(offset: Int): Short = {
    ((_buff(offset) & 0xFF) & 0xFFFF).toShort
  }

  /**
   *
   */
  def getShortLE(offset: Int): Short = {
    (((_buff(offset + 1) & 0xFF) << 8) |
      (_buff(offset) & 0xFF)).toShort
  }
  
  /**
   *
   */
  def getUShortLE(offset: Int): Int = {
    ((_buff(offset + 1) & 0xFF) << 8) & 0xFFFF |
      ((_buff(offset) & 0xFF) & 0xFFFF)
  }

  /**
   *
   */
  def getInt24LE(offset: Int): Int = {
    ((_buff(offset + 2) & 0xFF) << 16) |
      ((_buff(offset + 1) & 0xFF) << 8) |
      (_buff(offset) & 0xFF)
  }

  /**
   *
   */
  def getIntLE(offset: Int): Int = {
    ((_buff(offset + 3) & 0xFF) << 24) |
      ((_buff(offset + 2) & 0xFF) << 16) |
      ((_buff(offset + 1) & 0xFF) << 8) |
      (_buff(offset) & 0xFF)
  }

  /**
   *
   */
  def getInt48LE(offset: Int): Long = {
    ((_buff(offset + 5) & 0xFFL) << 40) |
      ((_buff(offset + 4) & 0xFFL) << 32) |
      ((_buff(offset + 3) & 0xFFL) << 24) |
      ((_buff(offset + 2) & 0xFFL) << 16) |
      ((_buff(offset + 1) & 0xFFL) << 8) |
      (_buff(offset) & 0xFFL)
  }

  /**
   *
   */
  def getUIntLE(offset: Int): Long = {
    ((_buff(offset + 3) & 0xFFL) << 24) |
      ((_buff(offset + 2) & 0xFFL) << 16) |
      ((_buff(offset + 1) & 0xFFL) << 8) |
      (_buff(offset) & 0xFFL)
  }

  /**
   *
   */
  def getLongLE(offset: Int): Long = {
    ((_buff(offset + 7) & 0xFFL) << 56) |
      ((_buff(offset + 6) & 0xFFL) << 48) |
      ((_buff(offset + 5) & 0xFFL) << 40) |
      ((_buff(offset + 4) & 0xFFL) << 32) |
      ((_buff(offset + 3) & 0xFFL) << 24) |
      ((_buff(offset + 2) & 0xFFL) << 16) |
      ((_buff(offset + 1) & 0xFFL) << 8) |
      (_buff(offset) & 0xFFL)
  }
  
  /**
   * 
   */
  def getDoubleLE(offset: Int): Double = {
    java.lang.Double.longBitsToDouble(getLongLE(offset))
  }

  /**
   *
   */
  def getShortBE(offset: Int): Short = {
    (((_buff(offset) & 0xFF) << 8) |
      (_buff(offset + 1) & 0xFF)).toShort
  }

  /**
   *
   */
  def getUShortBE(offset: Int): Int = {
    ((_buff(offset) & 0xFF) << 8) & 0xFFFF |
      ((_buff(offset + 1) & 0xFF) & 0xFFFF)
  }

  /**
   *
   */
  def getInt24BE(offset: Int): Int = {
    ((_buff(offset) & 0xFF) << 16) |
      ((_buff(offset + 1) & 0xFF) << 8) |
      (_buff(offset + 2) & 0xFF)
  }

  /**
   *
   */
  def getIntBE(offset: Int): Int = {
    ((_buff(offset) & 0xFF) << 24) |
      ((_buff(offset + 1) & 0xFF) << 16) |
      ((_buff(offset + 2) & 0xFF) << 8) |
      (_buff(offset + 3) & 0xFF)
  }

  /**
   *
   */
  def getInt48BE(offset: Int): Long = {
    ((_buff(offset) & 0xFFL) << 40) |
      ((_buff(offset + 1) & 0xFFL) << 32) |
      ((_buff(offset + 2) & 0xFFL) << 24) |
      ((_buff(offset + 3) & 0xFFL) << 16) |
      ((_buff(offset + 4) & 0xFFL) << 8) |
      (_buff(offset + 5) & 0xFFL)
  }

  /**
   *
   */
  def getUIntBE(offset: Int): Long = {
    ((_buff(offset) & 0xFFL) << 24) |
      ((_buff(offset + 1) & 0xFFL) << 16) |
      ((_buff(offset + 2) & 0xFFL) << 8) |
      (_buff(offset + 3) & 0xFFL)
  }

  /**
   *
   */
  def getLongBE(offset: Int): Long = {
    ((_buff(offset) & 0xFFL) << 56) |
      ((_buff(offset + 1) & 0xFFL) << 48) |
      ((_buff(offset + 2) & 0xFFL) << 40) |
      ((_buff(offset + 3) & 0xFFL) << 32) |
      ((_buff(offset + 4) & 0xFFL) << 24) |
      ((_buff(offset + 5) & 0xFFL) << 16) |
      ((_buff(offset + 6) & 0xFFL) << 8) |
      (_buff(offset + 7) & 0xFFL)
  }
  
  /**
   * 
   */
  def getDoubleBE(offset: Int): Double = {
    java.lang.Double.longBitsToDouble(getLongBE(offset))
  }

  /**
   *
   */
  def slice(offset: Int, length: Int): Array[Byte] = {
    _buff.slice(offset, offset + length)
  }

}