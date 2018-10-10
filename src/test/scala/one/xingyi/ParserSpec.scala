package one.xingyi

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers{
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

}
