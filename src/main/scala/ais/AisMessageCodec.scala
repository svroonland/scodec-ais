package ais

import java.time.{Instant, LocalDateTime, ZoneOffset}

import ais.AisDataTypeCodecs.{aisAsciiString, decimal}
import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs.{bits, bool, constant, discriminated, int8, intL, uint, _}
import shapeless.HNil

trait AisMessageCodec {
  //  def fromNmeaMessages(messages: Seq[String]): Attempt[AisMessage] =
  //    for {
  //      encoded <- Attempt.fromEither(BitVector.encodeString())
  //      nmea <- Codec[NmeaMessage].decodeValue(encoded)
  //      ais <- Codec[AisMessage].decodeValue(nmea.payload)
  //      _ = println(ais)
  //    } yield ais

  val etaCodec: Codec[Instant] =
    (uint(4) :: uint(5) :: uint(5) :: uint(6)).xmap[Instant](
      _.tupled match {
        case (month, day, hour, minute) =>
          Instant.from(
            LocalDateTime // TODO year
              .of(2018, month, day, hour, minute)
              .atZone(ZoneOffset.UTC))
      },
      instant => {
        val dt = instant.atZone(ZoneOffset.UTC)
        dt.getMonthValue :: dt.getDayOfMonth :: dt.getHour :: dt.getMinute :: HNil
      }
    )

  def withSentinel[T](sentinelValue: T, codec: Codec[T]): Codec[Option[T]] =
    codec.xmap(Some(_).filter(_ != sentinelValue), _ getOrElse sentinelValue)

  def lonI4Codec = {
    val lonIn10000thMinutes = decimal(28, 4, signed = true)
    val lonInDegrees = lonIn10000thMinutes.xmapc(_ / 60)(_ * 60)

    withSentinel(BigDecimal(181), lonInDegrees)
  }

  def latI4Codec = {
    val latIn10000thMinutes = decimal(27, 4, signed = true)
    val latInDegrees = latIn10000thMinutes.xmapc(_ / 60)(_ * 60)

    withSentinel(BigDecimal(91), latInDegrees)
  }

  val positionReportCodec: Codec[PositionReport] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("navigationStatus" | uint(4)) ::
      ("rateOfTurn" | withSentinel(-128, int8)) ::
      ("speedOverGround" | withSentinel(BigDecimal(511), decimal(10, 1))) ::
      ("positionAccuracy" | bool(1)) ::
      ("longitude" | lonI4Codec) ::
      ("latitude" | latI4Codec) ::
      ("courseOverGround" | withSentinel(BigDecimal(3600), decimal(12, 1))) ::
      ("trueHeading" | withSentinel(511, uint(9))) ::
      ("timestamp" | withSentinel(60, uint(6))) ::
      ("maneuverIndicator" | uint(2)) ::
      ("spare" | constant(BitVector.low(3))) ::
      ("raimFlag" | bool(1)) ::
      ("radioStatus" | uint(19))).as

  val aidToNavigationReportCodec: Codec[AidToNavigationReport] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("aidType" | uint(5)) ::
      ("vesselName" | aisAsciiString(20)) ::
      ("positionAccuracy" | bool(1)) ::
      ("longitude" | lonI4Codec) ::
      ("latitude" | latI4Codec) ::
      ("dimensionToBow" | uint(9)) ::
      ("dimensionToStern" | uint(9)) ::
      ("dimensionToPort" | uint(6)) ::
      ("dimensionToStarboard" | uint(6)) ::
      ("typeOfEPFD" | uint(4)) ::
      ("timestamp" | withSentinel(60, uint(6))) ::
      ("offPosition" | bool(1)) ::
      ("regionalReserved" | bits(8)) ::
      ("raimFlag" | bool(1)) ::
      ("virtualAidFlag" | bool(1)) ::
      ("assignedModeFlag" | bool(1))).as[AidToNavigationReport]

  val shipAndVoyageRelatedDataCodec: Codec[ShipAndVoyageRelatedData] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("aisVersion" | uint(2)) ::
      ("imoNumber" | uint(30)) ::
      ("callSign" | aisAsciiString(7)) ::
      ("vesselName" | aisAsciiString(20)) ::
      ("shipType" | uint(8)) ::
      ("dimensionToBow" | uint(9)) ::
      ("dimensionToStern" | uint(9)) ::
      ("dimensionToPort" | uint(6)) ::
      ("dimensionToStarboard" | uint(6)) ::
      ("positionFixType" | uint(4)) ::
      ("eta" | etaCodec) ::
      ("draught" | decimal(8, 1)) ::
      ("destination" | aisAsciiString(20)) ::
      ("dataTerminalReady" | bool(1)) <~ constant(BitVector.zero)).as

  implicit val codec: Codec[AisMessage] =
    discriminated[AisMessage]
      .by(intL(6))
      .subcaseO(1)(PartialFunction.condOpt(_) {
        case msg: PositionReport => msg
      })(positionReportCodec)
      .subcaseO(2)(PartialFunction.condOpt(_) {
        case msg: PositionReport => msg
      })(positionReportCodec)
      .subcaseO(3)(PartialFunction.condOpt(_) {
        case msg: PositionReport => msg
      })(positionReportCodec)
      .subcaseO(5)(PartialFunction.condOpt(_) {
        case msg: ShipAndVoyageRelatedData => msg
      })(shipAndVoyageRelatedDataCodec)
      .subcaseO(21)(PartialFunction.condOpt(_) {
        case msg: AidToNavigationReport => msg
      })(aidToNavigationReportCodec)
}
