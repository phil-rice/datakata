package one.xingyi

import org.scalatest.{FlatSpec, Matchers}

trait ParserFixture {
  implicit val theParser: Parser[String] = s => "<" + s + ">"

  val line1 = "junk1"
  val line2 = "junk2"
  val line3 = "012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789"
  val line4 = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789"
  val junkEnd = "junkEnd"

}

class ParserSpec extends KataSpec with ParserFixture with Maker{
  behavior of "implicit parsers"

  it should "have a parser for int" in {
    implicitly[Parser[Int]] apply "0" shouldBe 0
    implicitly[Parser[Int]] apply "3" shouldBe 3
    implicitly[Parser[Int]] apply " 3" shouldBe 3
  }
  it should "have a parser for string which trims the data" in {
    implicitly[Parser[String]] apply "0" shouldBe "0"
    implicitly[Parser[String]] apply "3" shouldBe "3"
    implicitly[Parser[String]] apply " 3" shouldBe "3"
  }
  behavior of "ColumnData"


  val c1_3 = ColumnData[String](1, 3)
  val c2_4 = ColumnData[String](2, 4)
  val raw = "abcdefghij"

  it should "extract data from a column to a column" in {
    c1_3(raw) shouldBe "<abc>"
    c2_4(raw) shouldBe "<bcd>"
  }

  behavior of "Maker"


  it should "allow a three column parser to be made" in {
    val p = parser value[String](1, 2) value[String](4, 5) value[String](7, 8) makeParser ((a, b, c) => s"$a/$b/$c")
    p(raw) shouldBe "<ab>/<de>/<gh>"

  }


}
