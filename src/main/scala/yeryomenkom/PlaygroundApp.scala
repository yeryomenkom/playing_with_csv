package yeryomenkom

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDateTime, ZoneOffset}

import yeryomenkom.Csv._
import yeryomenkom.Tick.{fromTuple, toTuple}

import scala.reflect.ClassTag

case class Tick(isin: String, timestamp: LocalDateTime, price: Double)

//lets define decoders/encoders that we will use in many places in our app
trait CsvSupport {
  implicit val LocalDateTimeCsvSegmentEncoder: CsvSegmentEncoder[LocalDateTime] =
    CsvSegmentEncoder.create(_.toString)
  implicit val LocalDateTimeCsvSegmentDecoder: CsvSegmentDecoder[LocalDateTime] =
    CsvSegmentDecoder.create(LocalDateTime.parse)
}

object Tick extends CsvSupport {
  val fromTuple: (String, LocalDateTime, Double) => Tick = Tick.apply _
  val toTuple: Tick => (String, LocalDateTime, Double) = (Tick.unapply _).andThen(_.get)

  //looks like a useless code...
  implicit val tickCsvDecoder: CsvDecoder[Tick] = CsvDecoder.from(fromTuple)
  implicit val tickCsvEncoder: CsvEncoder[Tick] = CsvEncoder.from(toTuple)
}

case class TickEnriched(isin: String, timestamp: LocalDateTime, price: Double, closePrice: Double)
//oh... looks too similar to the Tick encoders/decoders... But maybe I can live with it...
object TickEnriched extends CsvSupport {
  val fromTuple = TickEnriched.apply _
  val toTuple = (TickEnriched.unapply _).andThen(_.get)

  implicit val enrichedTickCsvDecoder: CsvDecoder[TickEnriched] = CsvDecoder.from(fromTuple)
  implicit val enrichedTickCsvEncoder: CsvEncoder[TickEnriched] = CsvEncoder.from(toTuple)
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
    implicit val localDateTimeSegmentEncoder: CsvSegmentEncoder[LocalDateTime] =
      CsvSegmentEncoder.create(_.toInstant(ZoneOffset.UTC).toEpochMilli.toString)
    implicit val localDateTimeSegmentDecoder: CsvSegmentDecoder[LocalDateTime] =
      CsvSegmentDecoder.create(str => LocalDateTime.ofInstant(Instant.ofEpochMilli(str.toLong), ZoneOffset.UTC))
    implicit val tickCsvDecoder: CsvDecoder[Tick] = CsvDecoder.from(fromTuple)
    implicit val tickCsvEncoder: CsvEncoder[Tick] = CsvEncoder.from(toTuple)

    //it works... but still looks too messy...
    checkSending(ticks)
  }
}
