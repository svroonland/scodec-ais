package nl.vroste.ais

import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._

private[ais] object BitVectorAsStringCodec {
  implicit val charset = java.nio.charset.StandardCharsets.US_ASCII

  /**
    * Encodes a bit vector as ASCII string
    */
  val sixBitEncodedString: Codec[BitVector] =
    string.xmap[BitVector](sixBitStringToBitVector, bitVectorToSixBitString(_))

  def sixBitCharToBitVector(c: Char): BitVector = {
    val int = if (c - 48 > 40) c - 48 - 8 else c - 48
    BitVector.fromInt(int, 8).drop(2)
  }

  def sixBitsToChar(bits: BitVector): Char =
    (bits.padLeft(2).toByte() + 0x48).toChar // TODO something like this?

  def sixBitStringToBitVector(s: String): BitVector =
    s.map(sixBitCharToBitVector).reduceLeft(_ ++ _)

  @scala.annotation.tailrec
  def bitVectorToSixBitString(bits: BitVector, acc: String = ""): String = {
    val (charBits, remaining) = bits.splitAt(6)
    bitVectorToSixBitString(remaining, acc + sixBitsToChar(charBits))
  }
}
