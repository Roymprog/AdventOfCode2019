package day12

class Energy {

  class System {
    val io = new Moon(new Position(4, 12, 13))
    val europa = new Moon(new Position(-9, 14, 3))
    val ganymede = new Moon(new Position(-7, -1, 2))
    val callisto = new Moon(new Position(-11, 17, -1))

    val moons = Array(io,europa,ganymede,callisto)

    def timeStep(n :Int): Array[Moon] = {
      moons.map(moon => moon.applyGravity(moons))
           .map(moon => moon.applyVelocity())
    }


  }

  class Moon(var position: Position, name:String="Unknown") {
    val velocity = new Velocity(0,0,0)
    def getPosition :Position = this.position

    // Gravity of itself will be 0 on all dimensions by default, so can be ignored
    def applyGravity(moons: Array[Moon]): Moon = {
      moons.foreach(moon => this.velocity.addVelocity(calcVelocity(moon)))
      this
    }

    def applyVelocity(): Moon = {
      this.position = this.position.move(this.velocity)
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
        getRelativePosition(this.position.getX, moon.position.getX),
        getRelativePosition(this.position.getY, moon.position.getY),
        getRelativePosition(this.position.getZ, moon.position.getZ),
        )
      new Velocity(x,y,z)
    }
  }

  class Position(x:Int, y:Int, z:Int) {
    def getX: Int = this.x
    def getY: Int = this.y
    def getZ: Int = this.z

    def move(vel : Velocity): Position = {
      new Position(getX + vel.x, getY + vel.y, getZ + vel.z)
    }
  }

  class Velocity(var x:Int, var y:Int, var z:Int) {
    def addVelocity(velocity: Velocity):Unit = {
      this.x = this.x + velocity.x
      this.y = this.y + velocity.y
      this.z = this.z + velocity.z
    }

  }
}
