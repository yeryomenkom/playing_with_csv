package yeryomenkom

import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.time.temporal.ChronoUnit

import yeryomenkom.Csv._

import scala.reflect.ClassTag

case class Tick(isin: String, timestamp: LocalDateTime, price: Double)

//lets define decoders/encoders and Isos that we will use in many places in our app
object CsvSupport {
  //these Isos... They look very similar...
  implicit val TickTupleIso: Iso[Tick, (String, LocalDateTime, Double)] =
    Iso((Tick.apply _).tupled, (Tick.unapply _).andThen(_.get))
  implicit val EnrichedTickTupleIso =
    Iso((TickEnriched.apply _).tupled, (TickEnriched.unapply _).andThen(_.get))

  implicit val LocalDateTimeCsvSegmentEncoder: CsvSegmentEncoder[LocalDateTime] =
    CsvSegmentEncoder.create(_.toString)
  implicit val LocalDateTimeCsvSegmentDecoder: CsvSegmentDecoder[LocalDateTime] =
    CsvSegmentDecoder.create(LocalDateTime.parse)
}

case class TickEnriched(isin: String, timestamp: LocalDateTime, price: Double, closePrice: Double)

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
  import CsvSupport._

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
    //We had to change names because of some implicit resolution rules
    implicit val LocalDateTimeCsvSegmentEncoder: CsvSegmentEncoder[LocalDateTime] =
      CsvSegmentEncoder.create(_.toInstant(ZoneOffset.UTC).toEpochMilli.toString)
    implicit val LocalDateTimeCsvSegmentDecoder: CsvSegmentDecoder[LocalDateTime] =
      CsvSegmentDecoder[Long].map(Instant.ofEpochMilli).map(LocalDateTime.ofInstant(_, ZoneOffset.UTC))

    checkSending(ticks)
  }
}
