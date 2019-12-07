package yeryomenkom

import scala.reflect.ClassTag
import scala.util.Try

object Csv {

  //define isomorphism abstraction
  case class Iso[A, B](from: B => A, to: A => B)

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

    implicit def deriveEncoder[A,T](implicit iso: Iso[A,T], encoder: CsvEncoder[T]): CsvEncoder[A] =
      encoder.contramap(iso.to)

    //hmm... still looks like some assembler lang... it would be nice if some machine could generate it...
    implicit def tuple3Encoder[X1,X2,X3](implicit
                                         x1Enc: CsvSegmentEncoder[X1],
                                         x2Enc: CsvSegmentEncoder[X2],
                                         x3Enc: CsvSegmentEncoder[X3]): CsvEncoder[(X1,X2,X3)] =
      create { case (x1, x2, x3) =>
        List(x1Enc.encode(x1), x2Enc.encode(x2), x3Enc.encode(x3))
      }

    implicit def tuple4Encoder[X1,X2,X3,X4](implicit
                                            x1Enc: CsvSegmentEncoder[X1],
                                            x2Enc: CsvSegmentEncoder[X2],
                                            x3Enc: CsvSegmentEncoder[X3],
                                            x4Enc: CsvSegmentEncoder[X4]): CsvEncoder[(X1,X2,X3,X4)] =
      create { case (x1, x2, x3, x4) =>
        List(x1Enc.encode(x1), x2Enc.encode(x2), x3Enc.encode(x3), x4Enc.encode(x4))
      }
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

    implicit def deriveDecoder[A,T](implicit iso: Iso[A,T], decoder: CsvDecoder[T]): CsvDecoder[A] =
      decoder.map(iso.from)

    implicit def tuple3Decoder[A,X1,X2,X3](implicit
                                           x1Dec: CsvSegmentDecoder[X1],
                                           x2Dec: CsvSegmentDecoder[X2],
                                           x3Dec: CsvSegmentDecoder[X3]): CsvDecoder[(X1,X2,X3)] =
      create { line =>
        val x1 :: x2 :: x3 :: Nil = line
        (x1Dec.decode(x1), x2Dec.decode(x2), x3Dec.decode(x3))
      }

    implicit def tuple4Decoder[A,X1,X2,X3,X4](implicit
                                              x1Dec: CsvSegmentDecoder[X1],
                                              x2Dec: CsvSegmentDecoder[X2],
                                              x3Dec: CsvSegmentDecoder[X3],
                                              x4Dec: CsvSegmentDecoder[X4]): CsvDecoder[(X1,X2,X3,X4)] =
      create { line =>
        val x1 :: x2 :: x3 :: x4 :: Nil = line
        (x1Dec.decode(x1), x2Dec.decode(x2), x3Dec.decode(x3), x4Dec.decode(x4))
      }
  }

  def decode[T: ClassTag](str: String, delimiter: String = DefaultCsvDelimiter)(implicit d: CsvDecoder[T]): Try[Seq[T]] =
    Try { str.split("\n").map(line => d.decode(line.split(delimiter).toList)) }

  def encode[T](values: Seq[T], delimiter: String = DefaultCsvDelimiter)(implicit e: CsvEncoder[T]): String =
    values.map(e.encode(_).mkString(delimiter)).mkString("\n")

}
