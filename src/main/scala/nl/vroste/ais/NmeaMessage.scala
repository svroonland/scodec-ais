package nl.vroste.ais

import scodec.bits.BitVector

case class NmeaMessage(
  nrFragments: Int,
  fragmentNr: Int,
  sequentialId: Option[Int],
  radioChannelCode: Char,
  payload: BitVector, // Decoded
  fillBits: Int
)
