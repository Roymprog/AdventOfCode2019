package day6

import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers
import day6.Orbits
import day6.Orbits.SolarSystem

class OrbitsTest extends FlatSpec with Matchers {
  "List of orbits" should "build solar system" in {
    val orbits = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nB)M\nB)N"
    val solarSystem =  new SolarSystem(orbits.split("\n").toList)
    solarSystem.com.orbitCount() shouldBe 46
  }

  "Equality" should "work" in {
    val name = "TST"
    new Orbits.Planet(name).equals(new Orbits.Planet(name)) shouldBe true
  }

  "List of planets" should "contains planet" in {
    val name = "TST"
    List(new Orbits.Planet(name)).contains(new Orbits.Planet(name)) shouldBe true
  }
}
