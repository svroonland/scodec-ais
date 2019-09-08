package ais

import org.scalatest._
import scodec.Attempt.{ Failure, Successful }
import scodec.{ Attempt, Codec }
import scodec.bits.BitVector

class AisCodecSpec
  extends WordSpec
  with Matchers
  with OptionValues
  with EitherValues
  with Inspectors
  with Inside
  with AisMessageCodec {
  implicit val charset = java.nio.charset.StandardCharsets.US_ASCII

  "Suffixed decoder" should {
    "decode properly" in {
      val aisMessage = BitVector
        .encodeString("!AIVDM,2,1,5,A,53KQ<L01pv7ptTtN22059@DlU>2222222222220l1@D555V00:R0CRkp8888,0*41")
        .right
        .get

      val codec = NmeaMessage.suffixed(scodec.codecs.string, NmeaMessage.suffix.toByte())

      inside(codec.decode(aisMessage)) {
        case Successful(scodec.DecodeResult(value, remainder)) =>
          value shouldBe "!AIVDM,2,1,5,A,53KQ<L01pv7ptTtN22059@DlU>2222222222220l1@D555V00:R0CRkp8888,0"
          remainder shouldBe BitVector.encodeString("41").right.get
      }
    }
  }

  def decodeNmeaMessage(msg: String): NmeaMessage =
    Codec[NmeaMessage]
      .decodeValue(BitVector.encodeString(msg).right.get) match {
      case Successful(value) => value
      case Failure(cause) =>
        throw new IllegalArgumentException(s"Failed to decode NMEA message ${cause}, ${cause}")
    }

  def assertAttempt[T](a: Attempt[T]) = {
    a should matchPattern {
      case Successful(_) =>
    }
  }

  def decodeAisMessage(message: String): Attempt[AisMessage] = {
    val encoded = BitVector.encodeString(message).right.value

    for {
      nmea <- Codec[NmeaMessage].decodeValue(encoded)
      ais  <- Codec[AisMessage].decodeValue(nmea.payload)
    } yield ais
  }

  "The AIS codec" should {
    "decode a PositionReport" in {
      val message = "!AIVDM,1,1,,A,13aIgm0P00PCtV6MdbFIROvP2>`<,0*4E"

      assertAttempt {
        decodeAisMessage(message).map { ais =>
          inside(ais) {
            case p: PositionReportClassA =>
              p.mmsi shouldBe 244740052
              p.navigationStatus shouldBe 0
              p.rateOfTurn shouldBe empty
              p.speedOverGround should contain(BigDecimal(0.0))
              p.positionAccuracy shouldBe true
              p.longitude should contain(BigDecimal("4.3574450"))
              p.latitude should contain(BigDecimal("51.900735"))
              p.courseOverGround should contain(BigDecimal(244.1))
              p.trueHeading shouldBe empty
              p.timestamp should contain(16)
              p.maneuverIndicator shouldBe 0
              p.raimFlag shouldBe true
          }
        }
      }
    }

    "decode a long range broadcast message" in {
      val message = "!AIVDM,1,1,,,KkAbld803WCfq8RL,0*72"

      assertAttempt {
        decodeAisMessage(message).map { ais =>
          inside(ais) {
            case p: LongRangeAisBroadcastMessage =>
              p.mmsi shouldBe 219854000
              p.positionAccuracy shouldBe true
              p.raimFlag shouldBe false
              p.navigationStatus shouldBe 0
              p.longitude should contain(BigDecimal("1.5417"))
              p.latitude should contain(BigDecimal("50.9633"))
              p.speedOverGround should contain(BigDecimal(17))
              p.courseOverGround should contain(BigDecimal(39))
              p.gnssPositionStatus shouldBe 0
          }
        }
      }

    }

    "decode and encode an AIS message" in {

      val nmeaMessages = Seq(
        "!AIVDM,2,1,5,A,53KQ<L01pv7ptTtN22059@DlU>2222222222220l1@D555V00:R0CRkp8888,0*41",
        "!AIVDM,2,2,5,A,88888888880,2*21"
      )

      val payload = nmeaMessages.map(decodeNmeaMessage).map(_.payload).reduce(_ ++ _)

      // Decode the payload
      val message = Codec[AisMessage].decodeValue(payload)
      inside(message) {
        case Successful(d: ShipAndVoyageRelatedData) =>
      }
    }

    "decode an AIS message without sequence number" in {
      val nmeaMessages = Seq("!AIVDM,1,1,,A,E>k`@f@:aPQRgQT6?b2ab000000<h1SJ9?KO800000lP00,4*4E")
        .map(decodeNmeaMessage)

      val payload = nmeaMessages.map(_.payload).reduce(_ ++ _)

      // Decode the payload
      val message = Codec[AisMessage].decodeValue(payload)
      inside(message) {
        case Successful(d: AidToNavigationReport) =>
      }
    }

    "decode more messages" in {
      forAll(messages) { msg =>
        withClue(msg) {
          val nmeaMessages = msg.lines.map(decodeNmeaMessage)

          val payload = nmeaMessages.map(_.payload).reduce(_ ++ _)

          // Decode the payload
          val message = Codec[AisMessage].decodeValue(payload)

          assertAttempt(message)
        }
      }

      succeed
    }

  }

  val messages = Seq(
    "!AIVDM,1,1,,A,E>k`@f@:aPQRgQT6?b2ab000000<h1SJ9?KO800000lP00,4*4E",
    "!AIVDM,1,1,,B,E>jMjbARHrnS0P000000000000006emR?Rf0hD2RRSv008,4*55",
    "!AIVDM,1,1,,A,13`jdd0P1RPL@R:MbIKsu?wD2>`<,0*6E",
    "!AIVDM,1,1,,A,13EbRlWP0PP1RbtEv?bpPwwD2>`<,0*25",
//    """!AIVDM,2,1,8,A,53ku2EP2;57t=4q@000DAD582Sko0023OV22,0*64
//      |!AIVM,2,2,8,A,221I:PF55v:ok03Sp888888888888888880,2*28""".stripMargin,
    """!AIVDM,2,1,,B,56K2?>@2BD7l`C?3GB1<PtDV0l59F2222222221I:`D6:5tU0@?RT82l,0*16
      |!AIVDM,2,2,8,B,kH3iQ`888888880,2*67""".stripMargin
  )
}
