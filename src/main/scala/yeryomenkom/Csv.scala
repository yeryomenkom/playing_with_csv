package yeryomenkom

import scala.reflect.ClassTag
import scala.util.Try
import shapeless._

object Csv {
  val DefaultCsvDelimiter = ","

  type CsvSegment = String
  type CsvLine = List[String]

  trait CsvSegmentEncoder[T] {
    def encode(value: T): CsvSegment
    //define an instance of CONTRAVARIANT FUNCTOR for CsvSegmentEncoder
    def contramap[B](f: B => T): CsvSegmentEncoder[B] = (value: B) => encode(f(value))
  }

  object CsvSegmentEncoder {
    def create[T](f: T => CsvSegment): CsvSegmentEncoder[T] = (value: T) => f(value)
    implicit val StringDecoder: CsvSegmentEncoder[String] = create(identity)
    //hmmm... too similar...
    implicit val IntDecoder: CsvSegmentEncoder[Int] = create(_.toString)
    implicit val LongDecoder: CsvSegmentEncoder[Long] = create(_.toString)
    implicit val DoubleDecoder: CsvSegmentEncoder[Double] = create(_.toString)
  }

  trait CsvSegmentDecoder[T] {
    def decode(value: CsvSegment): T
    //define an instance of COVARIANT FUNCTOR for CsvSegmentDecoder
    def map[B](f: T => B): CsvSegmentDecoder[B] = (value: CsvSegment) => f(decode(value))
  }

  object CsvSegmentDecoder {
    //just simple helper method that will be used instead of implicitly[CsvSegmentDecoder[A]]
    def apply[A](implicit decoder: CsvSegmentDecoder[A]): CsvSegmentDecoder[A] = decoder
    def create[T](f: CsvSegment => T): CsvSegmentDecoder[T] = (segment: CsvSegment) => f(segment)
    implicit val StringDecoder: CsvSegmentDecoder[String] = create(identity)
    implicit val IntDecoder: CsvSegmentDecoder[Int] = create(_.toInt)
    implicit val LongDecoder: CsvSegmentDecoder[Long] = create(_.toLong)
    implicit val DoubleDecoder: CsvSegmentDecoder[Double] = create(_.toDouble)
  }

  trait CsvEncoder[A] {
    def encode(x: A): CsvLine
    //define an instance of CONTRAVARIANT FUNCTOR for CsvEncoder
    def contramap[B](f: B => A): CsvEncoder[B] = (value: B) => encode(f(value))
  }

  object CsvEncoder {
    def create[A](f: A => CsvLine): CsvEncoder[A] = new CsvEncoder[A] {
      override def encode(x: A): CsvLine = f(x)
    }

    implicit def deriveCsvEncoder[T,R](implicit gen: Generic.Aux[T,R], genEncoder: CsvEncoder[R]): CsvEncoder[T] =
      genEncoder.contramap(gen.to)

    implicit val hnilEncoder: CsvEncoder[HNil] = (_: HNil) => List.empty

    implicit def hlistEncoder[H,T <: HList](implicit he: CsvSegmentEncoder[H], te: CsvEncoder[T]): CsvEncoder[H :: T] =
      (value: H :: T) => he.encode(value.head) +: te.encode(value.tail)
  }

  trait CsvDecoder[A] {
    def decode(line: CsvLine): A
    //define an instance of COVARIANT FUNCTOR for CsvDecoder
    def map[B](f: A => B): CsvDecoder[B] = (value: CsvLine) => f(decode(value))
  }

  object CsvDecoder {
    def create[A](f: CsvLine => A): CsvDecoder[A] = new CsvDecoder[A] {
      override def decode(line: CsvLine): A = f(line)
    }

    implicit def deriveCsvDecoder[T,R](implicit gen: Generic.Aux[T,R], genDecoder: CsvDecoder[R]): CsvDecoder[T] =
      genDecoder.map(gen.from)

    implicit val hnilDecoder: CsvDecoder[HNil] =
      (_: CsvLine) => HNil

    implicit def hlistDecoder[H,T <: HList](implicit hd: CsvSegmentDecoder[H], td: CsvDecoder[T]): CsvDecoder[H :: T] =
      (line: CsvLine) => hd.decode(line.head) :: td.decode(line.tail)
  }

  def decode[T: ClassTag](str: String, delimiter: String = DefaultCsvDelimiter)(implicit d: CsvDecoder[T]): Try[Seq[T]] =
    Try { str.split("\n").map(line => d.decode(line.split(delimiter).toList)) }

  def encode[T](values: Seq[T], delimiter: String = DefaultCsvDelimiter)(implicit e: CsvEncoder[T]): String =
    values.map(e.encode(_).mkString(delimiter)).mkString("\n")

}
