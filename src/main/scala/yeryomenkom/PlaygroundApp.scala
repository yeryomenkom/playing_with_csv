package yeryomenkom

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDateTime, ZoneOffset}

import yeryomenkom.Csv._

import scala.reflect.ClassTag

case class Tick(isin: String, timestamp: LocalDateTime, price: Double)

object Tick {
  implicit val tickCsvDecoder: CsvDecoder[Tick] = CsvDecoder.create { line =>
    val isinStr :: timestampStr :: priceStr :: Nil = line.split(',').toList
    Tick(isinStr, LocalDateTime.parse(timestampStr), priceStr.toDouble)
  }
  implicit val tickCsvEncoder: CsvEncoder[Tick] = CsvEncoder.create { tick =>
    List(tick.isin, tick.timestamp.toString, tick.price.toString).mkString(",")
  }
}

case class TickEnriched(isin: String, timestamp: LocalDateTime, price: Double, closePrice: Double)
//oh... looks too similar to the Tick encoders/decoders... But maybe I can live with it...
object TickEnriched {
  implicit val enrichedTickCsvDecoder: CsvDecoder[TickEnriched] = CsvDecoder.create { line =>
    val isinStr :: timestampStr :: priceStr :: closePriceStr :: Nil = line.split(',').toList
    TickEnriched(isinStr, LocalDateTime.parse(timestampStr), priceStr.toDouble, closePriceStr.toDouble)
  }
  implicit val enrichedTickCsvEncoder: CsvEncoder[TickEnriched] = CsvEncoder.create { tick =>
    List(tick.isin, tick.timestamp.toString, tick.price.toString, tick.closePrice.toString).mkString(",")
  }
}

object PlaygroundApp extends App {

  val ticks = Seq(
    Tick("ASOODDS", LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS), 0.12),
    Tick("ASOODDS", LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS), 12.02),
    Tick("ASOODDS", LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS), 123.129),
    Tick("ASOODDS", LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS), 8.97)
  )

  val enrichedTicks = ticks.map(tick =>
    TickEnriched(tick.isin, tick.timestamp, tick.price, 1.2)
  )

  val service = new Service

  service.checkTicksSendingForClientA(ticks)
  service.checkEnrichedTicksSendingForClientA(enrichedTicks)
  service.checkTicksSendingForClientB(ticks)
}

class Service {
  def checkSending[A: CsvEncoder: CsvDecoder: ClassTag](xs: Seq[A]): Unit = {
    val csvString = Csv.encode(xs)
    println(csvString)
    val decoded = Csv.decode[A](csvString)
    println(decoded)
    println(decoded.get == xs)
  }
  def checkTicksSendingForClientA(ticks: Seq[Tick]): Unit = checkSending(ticks)
  def checkEnrichedTicksSendingForClientA(ticks: Seq[TickEnriched]): Unit = checkSending(ticks)
  def checkTicksSendingForClientB(ticks: Seq[Tick]): Unit = {
    //ok. client B wants to receive timestamp in UNIX time
    implicit val tickCsvDecoderTweaked: CsvDecoder[Tick] = CsvDecoder.create { line =>
      val isinStr :: timestampStr :: priceStr :: Nil = line.split(',').toList
      Tick(isinStr, LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampStr.toLong), ZoneOffset.UTC), priceStr.toDouble)
    }
    implicit val tickCsvEncoderTweaked: CsvEncoder[Tick] = CsvEncoder.create { tick =>
      List(tick.isin, tick.timestamp.toInstant(ZoneOffset.UTC).toEpochMilli, tick.price.toString).mkString(",")
    }

    //it works... but it is to verbose... we have to fix it somehow!!!
    checkSending(ticks)
  }
}
