package one.xingyi

object Results extends App {
  println(FindSmallest[WeatherData](_.spread)(Ripper[WeatherData]("weather.dat")))
  println(FindSmallest[FootballData](_.spread)(Ripper[FootballData]("football.dat")))

}
