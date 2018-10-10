package one.xingyi

import java.io.File

import scala.io.Source

case class WeatherData(day: Int, maxTemp: Int, minTemp: Int) {
  def spread = maxTemp - minTemp
}

object WeatherData {

  import Maker._

  implicit val ParserForWeatherData = parser value[Int](3, 4) value[Int](7, 8) value[Int](13, 14) makeParser WeatherData.apply
}

case class ColumnData[C](fromColumn: Int, toColumn: Int)(implicit parser: Parser[C]) extends (String => C) {
  override def apply(raw: String): C = parser(raw.substring(fromColumn - 1, toColumn))
}

trait Parser[T] extends (String => T)

object Parser {
  //  implicit def parserString : Parser[String] = s => s
  implicit def toIntParser: Parser[Int] = { s: String => Integer.parseInt(s.trim) }
}

object Maker extends Maker

trait Maker {

  def parser = new ParserWord

  class ParserWord {

    case class value[C1: Parser](fromColumn: Int, toColumn: Int) {
      val c1 = ColumnData[C1](fromColumn, toColumn)

      case class value[C2: Parser](fromColumn: Int, toColumn: Int) {
        val c2 = ColumnData[C2](fromColumn, toColumn)

        case class value[C3: Parser](fromColumn: Int, toColumn: Int) {
          val c3 = ColumnData[C3](fromColumn, toColumn)

          def makeParser[T](fn: (C1, C2, C3) => T): Parser[T] = { raw: String =>
            fn(c1(raw), c2(raw), c3(raw))
          }
        }

      }

    }

  }

}

trait Ripper {
  def apply[T](s: Seq[String])(implicit parser: Parser[T]): Seq[T] = s.drop(2).dropRight(1).map(parser)

  def apply[T](s: String)(implicit parser: Parser[T]): Seq[T] = apply(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(s)).getLines().toSeq)
}

object Ripper extends Ripper

object FindSmallest {
  def apply[T](fn: T => Int)(s: Seq[T]): T = s.sortBy(fn).head
}

object Weather extends App {
  println(FindSmallest[WeatherData](_.spread)(Ripper[WeatherData]("weather.dat")))

}
