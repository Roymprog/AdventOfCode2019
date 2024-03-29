package day12

import day12.Energy.{Moon, Position, Velocity}
import day6.Orbits.SolarSystem
import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers

class EnergyTest extends FlatSpec with Matchers {

  "Applying gravity" should "work for 2 moon system" in {
    val io = new Moon(Position(-1,0,2), name="Io")
    val europa = new Moon(Position(2, -10, -7),name= "Europa")

    val system = new Energy.System(Array(io,europa))
    val Io = system.getMoon("Io").get
    val Europa = system.getMoon("Europa").get
    Io.applyGravity(system.moons)
    Io.velocity shouldBe Velocity(1,-1,-1)

    Europa.applyGravity(system.moons)
    Europa.velocity shouldBe Velocity(-1,1,1)

    Io.move()
    Io.position shouldBe Position(0,-1,1)

    Europa.move()
    Europa.position shouldBe Position(1,-9,-6)
  }

  "List of orbits" should "build solar system" in {
    val io = new Moon(Position(-1,0,2), name="Io")
    val europa = new Moon(Position(2, -10, -7),name= "Europa")
    val ganymede = new Moon(Position(4, -8, 8), name="Ganymede")
    val callisto = new Moon(Position(3,5,-1), name="Callisto")

    val system = new Energy.System(Array(io,europa,ganymede,callisto))
    system.timeStep(0)

    system.getMoon("Io").get.position shouldBe Energy.Position(-1,0,2)
    system.getMoon("Europa").get.position shouldBe Energy.Position(2,-10,-7)
    system.getMoon("Ganymede").get.position shouldBe Energy.Position(4,-8,8)
    system.getMoon("Callisto").get.position shouldBe Energy.Position(3,5,-1)

    system.timeStep(1)

    system.getMoon("Io").get.position shouldBe Energy.Position(2,-1,1)
    system.getMoon("Europa").get.position shouldBe Energy.Position(3,-7,-4)
    system.getMoon("Ganymede").get.position shouldBe Energy.Position(1,-7,5)
    system.getMoon("Callisto").get.position shouldBe Energy.Position(2,2,0)

    system.getMoon("Io").get.velocity shouldBe Energy.Velocity(3,-1,-1)
    system.getMoon("Europa").get.velocity shouldBe Energy.Velocity(1,3,3)
    system.getMoon("Ganymede").get.velocity shouldBe Energy.Velocity(-3,1,-3)
    system.getMoon("Callisto").get.velocity shouldBe Energy.Velocity(-1,-3,1)

    system.timeStep(1)

    system.getMoon("Io").get.position shouldBe Energy.Position(5,-3,-1)
    system.getMoon("Europa").get.position shouldBe Energy.Position(1,-2,2)
    system.getMoon("Ganymede").get.position shouldBe Energy.Position(1,-4,-1)
    system.getMoon("Callisto").get.position shouldBe Energy.Position(1,-4,2)

    system.getMoon("Io").get.velocity shouldBe Energy.Velocity(3,-2,-2)
    system.getMoon("Europa").get.velocity shouldBe Energy.Velocity(-2,5,6)
    system.getMoon("Ganymede").get.velocity shouldBe Energy.Velocity(0,3,-6)
    system.getMoon("Callisto").get.velocity shouldBe Energy.Velocity(-1,-6,2)

  }

  "Kinetic moon energy" should "be sum of velocity" in {
    val io = new Moon(Position(2,1,-3), Velocity(-3,-2,1),name="Io")
    val europa = new Moon(Position(1,-8,0), Velocity(-1,1,3),name= "Europa")
    val ganymede = new Moon(Position(3,-6,1), Velocity(3,2,-3), name="Ganymede")
    val callisto = new Moon(Position(2,0,4), Velocity(1,-1,-1), name="Callisto")

    val system = new Energy.System(Array(io,europa,ganymede,callisto))
    system.getTotalEnergy() shouldBe 179
  }

  "Total system energy" should "be 179" in {
    val io = new Moon(Position(-1,0,2), name="Io")
    val europa = new Moon(Position(2, -10, -7),name= "Europa")
    val ganymede = new Moon(Position(4, -8, 8), name="Ganymede")
    val callisto = new Moon(Position(3,5,-1), name="Callisto")

    val system = new Energy.System(Array(io, europa, ganymede, callisto))
    system.timeStep(10)
    system.getTotalEnergy() shouldBe 179
  }

  "System repeat" should "converge after 2772" in {
    val io = new Moon(Position(-1,0,2), name="Io")
    val europa = new Moon(Position(2, -10, -7),name= "Europa")
    val ganymede = new Moon(Position(4, -8, 8), name="Ganymede")
    val callisto = new Moon(Position(3,5,-1), name="Callisto")

    val system = new Energy.System(Array(io,europa,ganymede,callisto))

    system.repeatsDimensionAfter(2800) shouldBe (18,28, 44)
    system.sharedCycle(18,28,44) shouldBe 2772
  }

  "System repeat" should "converge after 4686774924" in {
    val io = new Moon(Position(-8,-10,0), name="Io")
    val europa = new Moon(Position(5, 5, 10),name= "Europa")
    val ganymede = new Moon(Position(2, -7, 3), name="Ganymede")
    val callisto = new Moon(Position(9,-8,-3), name="Callisto")

    val system = new Energy.System(Array(io,europa,ganymede,callisto))

    val (x,y,z) = system.repeatsDimensionAfter(4000,stepSize = 2)
//    val (x2,y2,z2) = system.repeatsDimensionAfter(1,stepSize = 2028)
//    val (x3,y3,z3) = system.repeatsDimensionAfter(1,stepSize = 2028)
    (x,y,z) shouldBe (2028,5898,4702)
    system.sharedCycle(x,y,z) shouldBe 4686774924L
  }
}
