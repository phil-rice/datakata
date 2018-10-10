package one.xingyi

import org.scalatest.{FlatSpec, Matchers}

class WeatherSpec extends FlatSpec with Matchers with Maker {
  implicit val theParser: Parser[String] = s => "<" + s + ">"

  behavior of "WeatherData"

  it should "have a spread method which is the difference between min and max temp" in {
    WeatherData(1, 20, 10).spread shouldBe 10
    WeatherData(1, 11, 10).spread shouldBe 1
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


  behavior of "parser with weather data"

  it should "turn a string of numbers into weather data" in {
    val p = parser value[Int](1, 2) value[Int](4, 5) value[Int](7, 8) makeParser (WeatherData.apply)

  }

  val line1 = "junk1"
  val line2 = "junk2"
  val line3 = "012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789"
  val line4 = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789"
  val junkEnd = "junkEnd"

  behavior of "ripper stream modifier for Weather Data"


  it should "drop the first n lines, the last line then pass the rest to the parser making objects" in {
    implicitly[RipperStreamModifier[WeatherData]] apply (Seq(line1, line2, line3, line4, junkEnd)) shouldBe Seq(line3, line4)
  }

  behavior of "Ripper stream modified for WeatherData"


  it should "drop the first two and the last" in {
    implicitly[RipperStreamModifier[WeatherData]] apply (Seq(line1, line2, line3, line4, junkEnd)) shouldBe Seq(line3, line4)
  }

  behavior of "ripper with real data"

  it should "read at least the first line of the weather data" in {
    Ripper[WeatherData]("weather.dat").take(1) shouldBe Seq(WeatherData(1, 88, 59))
  }

  behavior of "find smallest"

  it should "find the smallest" in {
    FindSmallest[String](_.toInt)(Seq("1", "2", "3")) shouldBe "1"
    FindSmallest[String](_.toInt)(Seq("1", "0", "3")) shouldBe "0"
  }


  behavior of "find smallest spread"

  it should "Find the smallest weather" in {
    FindSmallest[WeatherData](_.spread)(Ripper[WeatherData]("weather.dat")) shouldBe WeatherData(14, 61, 59)
  }

}
