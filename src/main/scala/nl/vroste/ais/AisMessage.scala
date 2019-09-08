package nl.vroste.ais

import java.time.Instant

import scodec.{ Attempt, Codec, Err }
import scodec.bits.{ BitVector, ByteVector }

sealed trait AisMessage

object AisMessage {
  import AttemptExtensions._
  import NmeaMessageCodec._
  import AisMessageCodec._

  private val charset = java.nio.charset.StandardCharsets.US_ASCII

  /**
    * Decode an AIS message from one or more NMEA messages
    *
    * @param nmeaStrings NMEA messages for a single AIS message
    * @return The decoded AIS message or a decoding error
    */
  def decode(nmeaStrings: String*): Either[Err, AisMessage] =
    (for {
      nmeaBits <- Attempt.successful(nmeaStrings.map(m => ByteVector(m.getBytes(charset)).bits))
      nmea     <- Attempt.traverse(nmeaBits)(nmeaMessageCodec.decodeValue)
      payload = nmea.map(_.payload).reduce(_ ++ _)
      aisMessage <- aisMessageCodec.decodeValue(payload)
    } yield aisMessage).toEither

  /**
    * Encode an AIS message as one or more NMEA messages
    *
    * @param message
    * @param sequentialId
    * @param radioChannelCode
    * @return
    */
  def encode(
    message: AisMessage,
    sequentialId: Option[Int] = None,
    radioChannelCode: Option[Char] = None
  ): Seq[String] = ???
}

final case class Dimensions(toBow: Int, toStern: Int, toPort: Int, toStarboard: Int)
object Dimensions {
  def create(toBow: Int, toStern: Int, toPort: Int, toStarboard: Int): Option[Dimensions] =
    if (toBow != 0 && toStern != 0 && toPort != 0 && toStarboard != 0)
      Some(Dimensions(toBow, toStern, toPort, toStarboard))
    else None
}

sealed trait PositionReport extends AisMessage {
  val repeatIndicator: Int
  val mmsi: Int
  val speedOverGround: Option[BigDecimal]
  val positionAccuracy: Boolean
  val longitude: Option[BigDecimal]
  val latitude: Option[BigDecimal]
  val courseOverGround: Option[BigDecimal]
  val raimFlag: Boolean
}

/**
  * Type 1, 2 and 3 Position Report class A
  */
final case class PositionReportClassA(
  repeatIndicator: Int,
  mmsi: Int,
  navigationStatus: Int,
  rateOfTurn: Option[Int],
  speedOverGround: Option[BigDecimal],
  positionAccuracy: Boolean,
  longitude: Option[BigDecimal],
  latitude: Option[BigDecimal],
  courseOverGround: Option[BigDecimal],
  trueHeading: Option[Int],
  timestamp: Option[Int],
  maneuverIndicator: Int,
  raimFlag: Boolean,
  radioStatus: Int
) extends PositionReport

final case class BinaryBroadcastMessage(
  repeatIndicator: Int,
  mmsi: Int,
  designatedAreaCode: Int,
  functionalId: Int,
  data: BitVector
) extends AisMessage

final case class ShipAndVoyageRelatedData(
  repeatIndicator: Int,
  mmsi: Int,
  aisVersion: Int,
  imoNumber: Int,
  callSign: String,
  vesselName: String,
  shipType: Int,
  dimensions: Option[Dimensions],
  positionFixType: Int,
  eta: Instant,
  draught: BigDecimal,
  destination: String,
  dataTerminalReady: Boolean
) extends AisMessage

/**
  * Type 18 Position Report
  */
final case class PositionReportClassB(
  repeatIndicator: Int,
  mmsi: Int,
  speedOverGround: Option[BigDecimal],
  positionAccuracy: Boolean,
  longitude: Option[BigDecimal],
  latitude: Option[BigDecimal],
  courseOverGround: Option[BigDecimal],
  trueHeading: Option[Int],
  timestamp: Option[Int],
  csUnit: Boolean,
  displayFlag: Boolean,
  dscFlag: Boolean,
  bandFlag: Boolean,
  message22Flag: Boolean,
  assigned: Boolean,
  raimFlag: Boolean,
  radioStatus: Int
) extends PositionReport

/**
  * Type 19 Position Report
  */
final case class PositionReportClassBExtended(
  repeatIndicator: Int,
  mmsi: Int,
  speedOverGround: Option[BigDecimal],
  positionAccuracy: Boolean,
  longitude: Option[BigDecimal],
  latitude: Option[BigDecimal],
  courseOverGround: Option[BigDecimal],
  trueHeading: Option[Int],
  timestamp: Option[Int],
  name: String,
  shipType: Int,
  dimensions: Option[Dimensions],
  positionFixType: Int,
  raimFlag: Boolean,
  dte: Boolean,
  assigned: Boolean
) extends PositionReport

final case class AidToNavigationReport(
  repeatIndicator: Int,
  mmsi: Int,
  aid_type: Int,
  name: String,
  positionAccuracy: Boolean,
  longitude: Option[BigDecimal],
  latitude: Option[BigDecimal],
  dimensions: Option[Dimensions],
  typeOfEPFD: Int,
  utcSecond: Option[Int],
  offPositionIndicator: Boolean,
  regionalReserved: BitVector,
  raimFlag: Boolean,
  virtualAidFlag: Boolean,
  assignedModeFlag: Boolean
) extends AisMessage

case class StaticDataReportPartA(
  repeatIndicator: Int,
  mmsi: Int,
  vesselName: String
) extends AisMessage

case class StaticDataReportPartB(
  repeatIndicator: Int,
  mmsi: Int,
  shipType: Int,
  vendorId: String,
  unitModelCode: Int,
  serialNumber: Int,
  callSign: String,
  dimensions: Option[Dimensions]
) extends AisMessage

/**
  * Type 27 Long range AIS broadcast message
  */
case class LongRangeAisBroadcastMessage(
  repeatIndicator: Int,
  mmsi: Int,
  positionAccuracy: Boolean,
  raimFlag: Boolean,
  navigationStatus: Int,
  longitude: Option[BigDecimal],
  latitude: Option[BigDecimal],
  speedOverGround: Option[BigDecimal],
  courseOverGround: Option[BigDecimal],
  gnssPositionStatus: Int
) extends PositionReport
