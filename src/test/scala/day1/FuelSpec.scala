package day1

import day1.Fuel
import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers


class FuelSpec extends FlatSpec with Matchers {
  "Calc fuel" should "return correct value" in {
    Fuel.calcFuel(12) shouldBe 2
  }

  "Calc fuel" should "round down" in {
    Fuel.calcFuel(14) shouldBe 2
  }

  "Calc fuel for fuel" should "zero" in {
    Fuel.extraFuel(2 ) shouldBe 0
  }

  "Calc fuel for zero fuel" should "zero" in {
    Fuel.extraFuel(0 ) shouldBe 0
  }

  "Calc fuel for fuel" should "return correct" in {
    Fuel.extraFuel(1969 ) shouldBe 966
  }

  "Calc fuel for fuel" should "return correct for big value" in {
    Fuel.extraFuel(100756 ) shouldBe 50346
  }

  "Calc fuel for big fuel" should "return correct" in {
    Fuel.extraFuel(3216744 ) shouldBe 1608329
  }
}
