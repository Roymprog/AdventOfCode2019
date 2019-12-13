package day12

object Energy {

  val io = new Moon(Position(4, 12, 13), name="Io")
  val europa = new Moon(Position(-9, 14, -3), name="Europa")
  val ganymede = new Moon(Position(-7, -1, 2), name="Ganymede")
  val callisto = new Moon(Position(-11, 17, -1), name= "Callisto")

  def run1() = {
    val system = new System(Array(io,europa,ganymede,callisto))
    println(s"The total energy of the system is ${system.timeStep(1000).getTotalEnergy()}")
  }

  class System(val moons: Array[Moon] ) {

    def timeStep(n :Int=0): System = {
      if (n <= 0) {
        this
      } else {
        this.moons.foreach(moon => moon.applyGravity(moons))
        this.moons.foreach(moon => moon.move())
        this.timeStep(n - 1)
      }
    }

    def getMoon(name:String) = {
      this.moons.find(moon => moon.name == name)
    }

    def getTotalEnergy() :Int = {
      this.moons.map(moon => moon.getPotentialEnergy()*moon.getKineticEnergy()).sum
    }
  }

  class Moon(var position: Position, var velocity: Velocity = Velocity(0,0,0), val name: String) {
    def getPosition :Position = this.position

    // Gravity of itself will be 0 on all dimensions by default, so can be ignored
    def applyGravity(moons: Array[Moon]): Moon = {
      moons.foreach(moon => this.speedUp(calcVelocity(moon)))
      this
    }

    private def calcVelocity(moon: Moon): Velocity = {
      def getRelativePosition(posA: Int, posB:Int): Int = {
        if (posA > posB) {
          -1
        } else if (posA == posB) {
          0
        } else {
          1
        }
      }
      val (x, y, z) = (
        getRelativePosition(this.position.x, moon.position.x),
        getRelativePosition(this.position.y, moon.position.y),
        getRelativePosition(this.position.z, moon.position.z),
        )
      Velocity(x,y,z)
    }

    def move(): Unit = {
      this.position = Position(this.position.x + this.velocity.x, this.position.y + this.velocity.y, this.position.z + this.velocity.z)
    }

    def speedUp(add: Velocity) :Unit = {
      this.velocity = Velocity(this.velocity.x + add.x, this.velocity.y + add.y, this.velocity.z + add.z)
    }

    def getPotentialEnergy():Int = {
      this.position.x.abs + this.position.y.abs + this.position.z.abs
    }

    def getKineticEnergy() :Int = {
      (this.velocity.x.abs + this.velocity.y.abs + this.velocity.z.abs)
    }
  }


  case class Position(x:Int, y:Int, z:Int) {
  }

  case class Velocity(x:Int,y:Int, z:Int) {
  }
}
