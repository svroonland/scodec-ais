# scodec-ais
Scala AIS message encoding and decoding library built with [scodec](http://scodec.org/)

# About
A typesafe library for decoding AIS messages. Special missing values (like 511 for true heading) are decoded as `Option` values. 

Currently a proof of concept, supporting decoding only of messages types 1, 2, 3, 5, 8, 18, 19, 21, 24, 27.

# Usage
```scala
import nl.vroste.ais._

val nmeaMessage = "!AIVDM,1,1,,A,13aIgm0P00PCtV6MdbFIROvP2>`<,0*4E" // This is a position report

val decodedOrError: Either[Err, AisMessage] = AisMessa.decode(nmeaMessage)

decodedOrError match {
  case(Some(positionReport)) =>
    println(positionReport.courseOverGround) // Will print 244.1
}

```

See the [unit tests](https://github.com/svroonland/scodec-ais/blob/master/src/test/scala/nl/vroste/ais/AisCodecSpec.scala) for more examples.


# Resources
* [AIVDM/AIVDO protocol decoding](https://gpsd.gitlab.io/gpsd/AIVDM.html)
