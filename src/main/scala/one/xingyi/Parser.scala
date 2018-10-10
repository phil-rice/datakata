package one.xingyi

case class ColumnData[C](fromColumn: Int, toColumn: Int)(implicit parser: Parser[C]) extends (String => C) {
  override def apply(raw: String): C = parser(raw.substring(fromColumn - 1, toColumn))
}

trait Parser[T] extends (String => T)

object Parser {
  implicit def parserString: Parser[String] = s => s.trim

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

