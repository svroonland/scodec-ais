package ais

import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._

object AisDataTypeCodecs {
  val sixBit = int(6)
  val aisAsciiChar: Codec[Char] = sixBit.xmap(toChar, fromChar)
  val padding = BitVector.low(6)

  // A decimal coded as integer with a fixed number of decimal places
  def decimal(length: Int,
              decimals: Int,
              signed: Boolean = false): Codec[BigDecimal] =
    (if (signed) int(length) else uint(length))
      .xmap[BigDecimal](i => BigDecimal.apply(i, decimals), _.setScale(-decimals).intValue())

  def aisAsciiString(length: Int): Codec[String] =
    paddedFixedSizeBits(6 * length, list(sixBit), constant(padding))
      .xmap(ints => new String(ints.takeWhile(_ != 0).map(toChar).toArray).trim,
        string => string.toCharArray.toList.map(fromChar))

  def toChar(i: Int): Char = (if (i < 32) i + 64 else i).toChar

  def fromChar(c: Char): Int = if (c >= 64) c - 64 else c
}
