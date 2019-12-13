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

  "List of planets" should "have child" in {
    val orbits = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nB)M\nB)N"
    val solarSystem =  new SolarSystem(orbits.split("\n").toList)
    solarSystem.com.hasChild("N") shouldBe true

    val D = solarSystem.com.getPlanet("D").get
    D.hasChild("I") shouldBe true
  }

  "Path to B" should "be COM-B" in {
    val orbits = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nB)M\nB)N"
    val solarSystem =  new SolarSystem(orbits.split("\n").toList)

    solarSystem.com.getPathTo("B").get shouldBe List("COM")

    solarSystem.com.getPathTo("K").get shouldBe List("COM", "B", "C", "D", "E", "J")
  }

  "Path to YOU" should "equal path ot SNT" in {
    val orbits = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
    val solarSystem =  new SolarSystem(orbits.split("\n").toList)

    solarSystem.com.getPathDiffs("YOU", "SAN") shouldBe 4
  }

}
