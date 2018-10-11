package one.xingyi

case class FootballData(name: String, forGoals: Int, againstGoals: Int) {
  def spread = Math.abs(forGoals - againstGoals)

}


object FootballData {
  import Maker._
  // "    8. Aston_Villa     38    12  14  12    46  -  47    50"
  //  123456789012345678901234567890123456789012345678901234567890
  implicit val footballParser = parser value[String](8, 22) value[Int](44, 45) value[Int](51, 52) makeParser (FootballData.apply)
  implicit val ripperForFootball: RipperStreamModifier[FootballData] = _.filterNot(_.trim.startsWith("-")).drop(1)
}