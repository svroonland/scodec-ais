package ais

import java.time.Instant

import scodec.bits.BitVector

sealed trait AisMessage

case class ShipAndVoyageRelatedData(repeatIndicator: Int,
                                    mmsi: Int,
                                    aisVersion: Int,
                                    imoNumber: Int,
                                    callSign: String,
                                    vesselName: String,
                                    shipType: Int,
                                    dimensionToBow: Int,
                                    dimensionToStern: Int,
                                    dimensionToPort: Int,
                                    dimensionToStarboard: Int,
                                    positionFixType: Int,
                                    eta: Instant,
                                    draught: BigDecimal,
                                    destination: String,
                                    dataTerminalReady: Boolean)
  extends AisMessage

case class AidToNavigationReport(repeatIndicator: Int,
                                 mmsi: Int,
                                 aid_type: Int,
                                 name: String,
                                 positionAccuracy: Boolean,
                                 longitude: Option[BigDecimal],
                                 latitude: Option[BigDecimal],
                                 dimensionToBow: Int,
                                 dimensionToStern: Int,
                                 dimensionToPort: Int,
                                 dimensionToStarboard: Int,
                                 typeOfEPFD: Int,
                                 utcSecond: Option[Int],
                                 offPositionIndicator: Boolean,
                                 regionalReserved: BitVector,
                                 raimFlag: Boolean,
                                 virtualAidFlag: Boolean,
                                 assignedModeFlag: Boolean)

  extends AisMessage

case class PositionReport(repeatIndicator: Int,
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
                          radioStatus: Int)
  extends AisMessage
