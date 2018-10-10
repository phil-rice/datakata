package one.xingyi

import org.scalatest.{FlatSpec, Matchers}

class WeatherSpec extends FlatSpec with Matchers with Maker with ParserFixture {

  behavior of "WeatherData"

  it should "have a spread method which is the difference between min and max temp" in {
    WeatherData(1, 20, 10).spread shouldBe 10
    WeatherData(1, 11, 10).spread shouldBe 1
  }
  behavior of "parser with weather data"

  it should "turn a string of numbers into weather data" in {
    val p = parser value[Int](1, 2) value[Int](4, 5) value[Int](7, 8) makeParser (WeatherData.apply)

  }

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
