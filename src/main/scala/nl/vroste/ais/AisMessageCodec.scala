package nl.vroste.ais

import java.time.{ Instant, LocalDateTime, ZoneOffset }

import AisDataTypeCodecs.{ aisAsciiString, decimal }
import scodec.Codec
import scodec.codecs.{ bits, bool, discriminated, int8, intL, uint, _ }
import shapeless.HNil

import scala.math.BigDecimal.RoundingMode
import scodec.bits._
private[ais] object AisMessageCodec {
  val etaCodec: Codec[Instant] =
    (uint(4) :: uint(5) :: uint(5) :: uint(6)).xmap[Instant](
      _.tupled match {
        case (month, day, hour, minute) =>
          Instant.from(
            LocalDateTime // TODO year
              .of(2018, month, day, hour, minute)
              .atZone(ZoneOffset.UTC)
          )
      },
      instant => {
        val dt = instant.atZone(ZoneOffset.UTC)
        dt.getMonthValue :: dt.getDayOfMonth :: dt.getHour :: dt.getMinute :: HNil
      }
    )

  def noneWhen[T](sentinelValue: T, codec: Codec[T]): Codec[Option[T]] =
    codec.xmap(Some(_).filter(_ != sentinelValue), _ getOrElse sentinelValue)

  def lonI4Codec = {
    val lonIn10000thMinutes = decimal(28, 4, signed = true)
    val lonInDegrees        = lonIn10000thMinutes.xmapc(_ / 60)(_ * 60)

    noneWhen(BigDecimal(181), lonInDegrees)
  }

  def latI4Codec = {
    val latIn10000thMinutes = decimal(27, 4, signed = true)
    val latInDegrees        = latIn10000thMinutes.xmapc(_ / 60)(_ * 60)

    noneWhen(BigDecimal(91), latInDegrees)
  }

  val positionReportCodec: Codec[PositionReportClassA] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("navigationStatus" | uint(4)) ::
      ("rateOfTurn" | noneWhen(-128, int8)) ::
      ("speedOverGround" | noneWhen(BigDecimal(511), decimal(10, 1))) ::
      ("positionAccuracy" | bool(1)) ::
      ("longitude" | lonI4Codec) ::
      ("latitude" | latI4Codec) ::
      ("courseOverGround" | noneWhen(BigDecimal(3600), decimal(12, 1))) ::
      ("trueHeading" | noneWhen(511, uint(9))) ::
      ("timestamp" | noneWhen(60, uint(6))) ::
      ("maneuverIndicator" | uint(2)) ::
      ("spare" | ignore(3)) ::
      ("raimFlag" | bool(1)) ::
      ("radioStatus" | uint(19))).as

  val dimensionCodec: Codec[Option[Dimensions]] =
    (("dimensionToBow" | uint(9)) ::
      ("dimensionToStern" | uint(9)) ::
      ("dimensionToPort" | uint(6)) ::
      ("dimensionToStarboard" | uint(6)))
      .xmapc(x => (Dimensions.create _).tupled(x.tupled))(
        _.map(d => d.toBow :: d.toStern :: d.toPort :: d.toStarboard :: HNil).getOrElse(0 :: 0 :: 0 :: 0 :: HNil)
      )

  val shipAndVoyageRelatedDataCodec: Codec[ShipAndVoyageRelatedData] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("aisVersion" | uint(2)) ::
      ("imoNumber" | uint(30)) ::
      ("callSign" | aisAsciiString(7)) ::
      ("vesselName" | aisAsciiString(20)) ::
      ("shipType" | uint(8)) ::
      dimensionCodec ::
      ("positionFixType" | uint(4)) ::
      ("eta" | etaCodec) ::
      ("draught" | decimal(8, 1)) ::
      ("destination" | aisAsciiString(20)) ::
      ("dataTerminalReady" | bool(1)) <~ ignore(1)).as

  val binaryBroadcastMessageCodec: Codec[BinaryBroadcastMessage] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("spare" | ignore(3)) ::
      ("designatedAreaCode" | uint(10)) ::
      ("functionalId" | uint(6)) ::
      ("data" | bits)).as[BinaryBroadcastMessage]

  val positionReportClassBCodec: Codec[PositionReportClassB] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("regionalReserved" | ignore(8)) ::
      ("speedOverGround" | noneWhen(BigDecimal(511), decimal(10, 1))) ::
      ("positionAccuracy" | bool(1)) ::
      ("longitude" | lonI4Codec) ::
      ("latitude" | latI4Codec) ::
      ("courseOverGround" | noneWhen(BigDecimal(3600), decimal(12, 1))) ::
      ("trueHeading" | noneWhen(511, uint(9))) ::
      ("timestamp" | noneWhen(60, uint(6))) ::
      ("regionalReserved" | ignore(2)) ::
      ("csUnit" | bool(1)) ::
      ("displayFlag" | bool(1)) ::
      ("dscFlag" | bool(1)) ::
      ("bandFlag" | bool(1)) ::
      ("message22Flag" | bool(1)) ::
      ("assignedFlag" | bool(1)) ::
      ("raimFlag" | bool(1)) ::
      ("radioStatus" | uint(20))).as[PositionReportClassB]

  val positionReportClassBExtendedCodec: Codec[PositionReportClassBExtended] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("regionalReserved" | ignore(8)) ::
      ("speedOverGround" | noneWhen(BigDecimal(511), decimal(10, 1))) ::
      ("positionAccuracy" | bool(1)) ::
      ("longitude" | lonI4Codec) ::
      ("latitude" | latI4Codec) ::
      ("courseOverGround" | noneWhen(BigDecimal(3600), decimal(12, 1))) ::
      ("trueHeading" | noneWhen(511, uint(9))) ::
      ("timestamp" | noneWhen(60, uint(6))) ::
      ("regionalReserved" | ignore(4)) ::
      ("name" | aisAsciiString(20)) ::
      ("shipType" | uint(8)) ::
      ("dimensions" | dimensionCodec) ::
      ("positionFixType" | uint(4)) ::
      ("raimFlag" | bool(1)) ::
      ("dte" | bool(1)) ::
      ("assignedFlag" | bool(1))).as[PositionReportClassBExtended]

  val aidToNavigationReportCodec: Codec[AidToNavigationReport] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("aidType" | uint(5)) ::
      ("vesselName" | aisAsciiString(20)) ::
      ("positionAccuracy" | bool(1)) ::
      ("longitude" | lonI4Codec) ::
      ("latitude" | latI4Codec) ::
      dimensionCodec ::
      ("typeOfEPFD" | uint(4)) ::
      ("timestamp" | noneWhen(60, uint(6))) ::
      ("offPosition" | bool(1)) ::
      ("regionalReserved" | bits(8)) ::
      ("raimFlag" | bool(1)) ::
      ("virtualAidFlag" | bool(1)) ::
      ("assignedModeFlag" | bool(1))).as[AidToNavigationReport]

  val staticDataReportPartACodec: Codec[StaticDataReportPartA] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("partNo" | constant(bin"00")) ::
      ("vesselName" | aisAsciiString(20))).as

  val staticDataReportPartBCodec: Codec[StaticDataReportPartB] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("partNo" | constant(bin"01")) ::
      ("shipType" | uint(8)) ::
      ("vendorId" | aisAsciiString(3)) ::
      ("unitModelCode" | uint(4)) ::
      ("serialNumber" | uint(20)) ::
      ("callSign" | aisAsciiString(7)) ::
      dimensionCodec ::
      ("spare" | constantLenient(bin"000000"))).as

  val longRangeBroadcastMessageCodec: Codec[LongRangeAisBroadcastMessage] =
    (("repeatIndicator" | uint(2)) ::
      ("mmsi" | uint(30)) ::
      ("positionAccuracy" | bool(1)) ::
      ("raimFlag" | bool(1)) ::
      ("navigationStatus" | uint(4)) ::
      ("longitude" | {
        val lonIn10thMinutes = decimal(18, 1, signed = true)
        val lonInDegrees     = lonIn10thMinutes.xmapc(i => ((i / 60).setScale(4, RoundingMode.HALF_UP)))(_ * 60)

        noneWhen(BigDecimal(181), lonInDegrees)
      }) ::
      ("latitude" | {
        val latIn10thMinutes = decimal(17, 1, signed = true)
        val latInDegrees     = latIn10thMinutes.xmapc(i => (i / 60).setScale(4, RoundingMode.HALF_UP))(_ * 60)

        noneWhen(BigDecimal(91), latInDegrees)
      }) ::
      ("speedOverGround" | noneWhen(BigDecimal(63), decimal(6, 0, signed = true))) ::
      ("courseOverGround" | noneWhen(BigDecimal(511), decimal(9, 0, signed = true))) ::
      ("gnssPositionStatus" | int(1)) ::
      ("spare" | constantLenient(bin"0"))).as[LongRangeAisBroadcastMessage]

  implicit val aisMessageCodec: Codec[AisMessage] =
    discriminated[AisMessage]
      .by(intL(6))
      .subcaseO(1)(_.asOpt[PositionReportClassA])(positionReportCodec)
      .subcaseO(2)(_.asOpt[PositionReportClassA])(positionReportCodec)
      .subcaseO(3)(_.asOpt[PositionReportClassA])(positionReportCodec)
      .subcaseO(5)(_.asOpt[ShipAndVoyageRelatedData])(shipAndVoyageRelatedDataCodec)
      .subcaseO(8)(_.asOpt[BinaryBroadcastMessage])(binaryBroadcastMessageCodec)
      .subcaseO(18)(_.asOpt[PositionReportClassB])(positionReportClassBCodec)
      .subcaseO(19)(_.asOpt[PositionReportClassBExtended])(positionReportClassBExtendedCodec)
      .subcaseO(21)(_.asOpt[AidToNavigationReport])(aidToNavigationReportCodec)
      .subcaseP[AisMessage](24) {
        case msg: StaticDataReportPartA => msg
        case msg: StaticDataReportPartB => msg
      }(
        choice(staticDataReportPartACodec.upcast[AisMessage], staticDataReportPartBCodec.upcast[AisMessage])
      )
      .subcaseO(27)(_.asOpt[LongRangeAisBroadcastMessage])(longRangeBroadcastMessageCodec)
}
