package one.xingyi

import org.scalatest.{FlatSpec, Matchers}

class FootballSpec extends KataSpec {

  behavior of "FootballData"

  val f1 = FootballData("f1", 5, 10)
  val f2 = FootballData("f1", 22, 10)
  it should "have a spread that is the for and against difference" in {
    f1.spread shouldBe 5
    f2.spread shouldBe 12
  }

  behavior of "Football data parser"

  it should "rip out the name, for and against" in {
    val s = "    8. Aston_Villa     38    12  14  12    46  -  47    50"
    implicitly[Parser[FootballData]] apply s shouldBe FootballData("Aston_Villa", 46, 47)
  }

  behavior of "Ripper stream modifier for football"

  val l1 = "line1"
  val l2 = "line2"
  val l3 = "line3"
  val l4 = "line4"
  val lLines = "--"
  val lLines2 = "   --"
  it should "drop the first line and ignore lines starting with  whitespace then -" in {
    implicitly[RipperStreamModifier[FootballData]] apply (Seq(l1, lLines, l2, lLines2, l3, l4)) shouldBe Seq(l2, l3, l4)
  }

  behavior of "Loading football file and printing spread"

  it should "have the  first line as" in {
    FindSmallest[FootballData](_.spread)(Ripper[FootballData]("football.dat")) shouldBe FootballData("Aston_Villa",46,47)
  }
}
