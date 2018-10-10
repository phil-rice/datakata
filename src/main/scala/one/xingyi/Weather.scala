package one.xingyi

import java.io.File

import scala.io.Source

case class WeatherData(day: Int, maxTemp: Int, minTemp: Int) {
  def spread = maxTemp - minTemp
}

object WeatherData {

  import Maker._

  implicit val ParserForWeatherData = parser value[Int](3, 4) value[Int](7, 8) value[Int](13, 14) makeParser WeatherData.apply

  implicit val ripperSteamModifierForWeatherData: RipperStreamModifier[WeatherData] = _.drop(2).dropRight(1)
}







