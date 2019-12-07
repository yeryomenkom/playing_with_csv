package yeryomenkom

import scala.reflect.ClassTag
import scala.util.Try

object Csv {

  val DefaultCsvDelimiter = ","

  type CsvSegment = String
  type CsvLine = List[String]

  trait CsvSegmentEncoder[T] {
    def encode(value: T): CsvSegment
  }

  object CsvSegmentEncoder {
    def create[T](f: T => CsvSegment): CsvSegmentEncoder[T] = (value: T) => f(value)
    implicit val StringDecoder: CsvSegmentEncoder[String] = create(identity)
    implicit val IntDecoder: CsvSegmentEncoder[Int] = create(_.toString)
    implicit val LongDecoder: CsvSegmentEncoder[Long] = create(_.toString)
    implicit val DoubleDecoder: CsvSegmentEncoder[Double] = create(_.toString)
  }

  trait CsvSegmentDecoder[T] {
    def decode(value: CsvSegment): T
  }

  object CsvSegmentDecoder {
    def create[T](f: CsvSegment => T): CsvSegmentDecoder[T] = (segment: CsvSegment) => f(segment)
    implicit val StringDecoder: CsvSegmentDecoder[String] = create(identity)
    implicit val IntDecoder: CsvSegmentDecoder[Int] = create(_.toInt)
    implicit val LongDecoder: CsvSegmentDecoder[Long] = create(_.toLong)
    implicit val DoubleDecoder: CsvSegmentDecoder[Double] = create(_.toDouble)
  }

  trait CsvEncoder[A] {
    def encode(x: A): CsvLine
  }

  object CsvEncoder {
    def create[A](f: A => CsvLine): CsvEncoder[A] = new CsvEncoder[A] {
      override def encode(x: A): CsvLine = f(x)
    }
    //hmm... looks like some assembler lang... it would be nice if some machine could generate it...
    def from[A,X1,X2,X3](f: A => (X1,X2,X3))
                        (implicit
                         x1Enc: CsvSegmentEncoder[X1],
                         x2Enc: CsvSegmentEncoder[X2],
                         x3Enc: CsvSegmentEncoder[X3]): CsvEncoder[A] =
      create { a =>
        val (x1, x2, x3) = f(a)
        List(x1Enc.encode(x1), x2Enc.encode(x2), x3Enc.encode(x3))
      }
    def from[A,X1,X2,X3,X4](f: A => (X1,X2,X3,X4))
                           (implicit
                            x1Enc: CsvSegmentEncoder[X1],
                            x2Enc: CsvSegmentEncoder[X2],
                            x3Enc: CsvSegmentEncoder[X3],
                            x4Enc: CsvSegmentEncoder[X4]): CsvEncoder[A] =
      create { a =>
        val (x1, x2, x3, x4) = f(a)
        List(x1Enc.encode(x1), x2Enc.encode(x2), x3Enc.encode(x3), x4Enc.encode(x4))
      }
  }

  trait CsvDecoder[A] {
    def decode(line: CsvLine): A
  }

  object CsvDecoder {
    def create[A](f: CsvLine => A): CsvDecoder[A] = new CsvDecoder[A] {
      override def decode(line: CsvLine): A = f(line)
    }
    def from[A,X1,X2,X3](f: (X1,X2,X3) => A)
                           (implicit
                            x1Dec: CsvSegmentDecoder[X1],
                            x2Dec: CsvSegmentDecoder[X2],
                            x3Dec: CsvSegmentDecoder[X3]): CsvDecoder[A] =
      create { line =>
        val x1 :: x2 :: x3 :: Nil = line
        f(x1Dec.decode(x1), x2Dec.decode(x2), x3Dec.decode(x3))
      }
    def from[A,X1,X2,X3,X4](f: (X1,X2,X3,X4) => A)
                           (implicit
                            x1Dec: CsvSegmentDecoder[X1],
                            x2Dec: CsvSegmentDecoder[X2],
                            x3Dec: CsvSegmentDecoder[X3],
                            x4Dec: CsvSegmentDecoder[X4]): CsvDecoder[A] =
      create { line =>
        val x1 :: x2 :: x3 :: x4 :: Nil = line
        f(x1Dec.decode(x1), x2Dec.decode(x2), x3Dec.decode(x3), x4Dec.decode(x4))
      }
  }

  def decode[T: ClassTag](str: String, delimiter: String = DefaultCsvDelimiter)(implicit d: CsvDecoder[T]): Try[Seq[T]] =
    Try { str.split("\n").map(line => d.decode(line.split(delimiter).toList)) }

  def encode[T](values: Seq[T], delimiter: String = DefaultCsvDelimiter)(implicit e: CsvEncoder[T]): String =
    values.map(e.encode(_).mkString(delimiter)).mkString("\n")

}
