package ais

import scodec.{ Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound }
import scodec.bits.{ BitVector, ByteVector }
import scodec.codecs._
import BitVectorAsStringCodec._

import scala.util.Try

case class NmeaMessage(
  nrFragments: Int,
  fragmentNr: Int,
  sequentialId: Option[Int],
  radioChannelCode: Char,
  payload: BitVector, // Decoded
  fillBits: Int
)

object NmeaMessage {

//  def fromPayload(payload: BitVector, sequentialId: Option[Int], radioChannelCode: Char): Seq[NmeaMessage] = {
//
//  }

  implicit val charset = java.nio.charset.StandardCharsets.US_ASCII

  val commaBits = BitVector.encodeString(",").right.get
  val suffix    = BitVector.encodeString("*").right.get

  val `,` = "comma" | constant(commaBits)

  def parseInt(s: String) = Attempt.fromOption(Try(s.toInt).toOption, Err(s"Not an integer: ${s}"))

  val intEncodedAsChar: Codec[Int] = limitedSizeBytes(1, string).exmapc(parseInt)(i => Attempt.successful(i.toString))
  val char: Codec[Char] =
    limitedSizeBytes(1, string).xmap(_.charAt(0), _.toString)

  val payloadCodec = sixBitEncodedString

  def codecOrValue[A](codec: Codec[A], value: A) = {
    val decoder = Decoder(bits => codec.asDecoder.decode(bits).orElse(Decoder.point(value).decode(bits)))

    Codec(codec, decoder)
  }

  val radioChannelCodec: Codec[Char] = char.exmapc(
    v => if (v == 'A' || v == 'B') Attempt.successful(v) else Attempt.failure(Err("Invalid radio channel code"))
  )(Attempt.successful)

  val sentence =
    constant(ByteVector.encodeString("AIVDM").right.get) ~> `,` ~>
      (("nrFragments" | intEncodedAsChar) <~ `,`) ~
      (("fragmentNr" | intEncodedAsChar) <~ `,`) ~
      (("seqNr" | codecOrValue("seqNrDef" | intEncodedAsChar, 1)).widenOptc(Option.apply)(identity) <~ `,`) ~
      (("radioChannelCode" | codecOrValue(radioChannelCodec, 'A')) <~ `,`) ~
      (("payload" | suffixed(sixBitEncodedString, ','))) ~
      ("fillBits" | intEncodedAsChar)

  implicit val codec: Codec[NmeaMessage] =
    constant(ByteVector.encodeString("!").right.get) ~>
      ("checksummed" | checksummed(
        target = sentence,
        checksum = calculateChecksum,
        framing = suffixed(bits, suffix.toByte()) ~ bits,
        validate = true
      )).flattenLeftPairs
        .as[NmeaMessage]

  def calculateChecksum(bitVector: BitVector): BitVector = {
    val bytes          = bitVector.toByteVector.toSeq
    val checksum       = bytes.foldLeft(0) { case (a, b) => a ^ b.toInt }
    val checksumString = checksum.toHexString.toUpperCase
    BitVector.encodeString(checksumString).right.get
  }

  /**
    * Codec that encodes a value with a suffix
    */
  def suffixed[A](inner: Codec[A], suffix: Byte) = new Codec[A] {
    override def sizeBound: SizeBound = inner.sizeBound + SizeBound.exact(8)

    override def encode(value: A): Attempt[BitVector] =
      inner
        .encode(value)
        .map(_ ++ BitVector(suffix))

    override def decode(bits: BitVector): Attempt[DecodeResult[A]] =
      (for {
        _ <- Decoder.get
        untilEnd = bits.bytes.takeWhile(_ != suffix).bits
        _     <- Decoder.set(untilEnd)
        value <- inner
        _     <- Decoder.set(bits.drop(untilEnd.size + 8))
      } yield value).decode(bits)
  }

}
