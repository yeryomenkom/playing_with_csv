package yeryomenkom

import scala.reflect.ClassTag
import scala.util.Try

object Csv {

  type CsvLine = String

  trait CsvEncoder[A] {
    def encode(x: A): CsvLine
  }

  trait CsvDecoder[A] {
    def decode(line: CsvLine): A
  }

  def decode[T: ClassTag](csvString: String)(implicit d: CsvDecoder[T]): Try[Seq[T]] =
    Try { csvString.split("\n").map(line => d.decode(line)) }

  def encode[T](values: Seq[T])(implicit e: CsvEncoder[T]): String =
    values.map(e.encode).mkString("\n")

}
