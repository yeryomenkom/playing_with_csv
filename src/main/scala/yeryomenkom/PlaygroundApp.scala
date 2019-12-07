package yeryomenkom

import java.time.LocalDateTime

import yeryomenkom.Csv._

case class Tick(isin: String, timestamp: LocalDateTime, price: Double)

object PlaygroundApp extends App {

  val ticks = Seq(
    Tick("ASOODDS", LocalDateTime.now(), 0.12),
    Tick("ASOODDS", LocalDateTime.now(), 12.02),
    Tick("ASOODDS", LocalDateTime.now(), 123.129),
    Tick("ASOODDS", LocalDateTime.now(), 8.97)
  )

  implicit val tickCsvDecoder: CsvDecoder[Tick] = new CsvDecoder[Tick] {
    override def decode(line: CsvLine): Tick = {
      val isinStr :: timestampStr :: priceStr :: Nil = line.split(',').toList
      Tick(isinStr, LocalDateTime.parse(timestampStr), priceStr.toDouble)
    }
  }

  implicit val tickCsvEncoder: CsvEncoder[Tick] = new CsvEncoder[Tick] {
    override def encode(x: Tick): CsvLine =
      List(x.isin, x.timestamp.toString, x.price.toString).mkString(",")
  }

  val csvString = Csv.encode(ticks)
  println(csvString)
  val decoded = Csv.decode[Tick](csvString)
  println(decoded)
  println(decoded.get == ticks)
}
