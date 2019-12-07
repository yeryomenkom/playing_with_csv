package yeryomenkom

import java.time.LocalDateTime

import yeryomenkom.Csv._

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

object PlaygroundApp extends App {

  val ticks = Seq(
    Tick("ASOODDS", LocalDateTime.now(), 0.12),
    Tick("ASOODDS", LocalDateTime.now(), 12.02),
    Tick("ASOODDS", LocalDateTime.now(), 123.129),
    Tick("ASOODDS", LocalDateTime.now(), 8.97)
  )

  val csvString = Csv.encode(ticks)
  println(csvString)
  val decoded = Csv.decode[Tick](csvString)
  println(decoded)
  println(decoded.get == ticks)
}
