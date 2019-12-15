package day12

object Energy {


  def run1() = {
    val io = new Moon(Position(4, 12, 13), name="Io")
    val europa = new Moon(Position(-9, 14, -3), name="Europa")
    val ganymede = new Moon(Position(-7, -1, 2), name="Ganymede")
    val callisto = new Moon(Position(-11, 17, -1), name= "Callisto")

    val system = new System(Array(io,europa,ganymede,callisto))
    println(s"The total energy of the system is ${system.timeStep(1000).getTotalEnergy()}")
  }

  def run2() = {
    val io = new Moon(Position(4,12,13), name="Io")
    val europa = new Moon(Position(-9,14,-3), name="Europa")
    val ganymede = new Moon(Position(-7,-1,2), name="Ganymede")
    val callisto = new Moon(Position(-11,17,-1), name= "Callisto")

    val system = new System(Array(io,europa,ganymede,callisto))
//    step to z
//    system.repeatsDimensionAfter(1,stepSize = 96236)
//    val (x, y, z) = system.repeatsDimensionAfter(1000000, startCount = 96236)
    println(s"The total cycle is ${system.sharedCycle(167624, 231614, 96236)}")
  }

  class System(val moons: Array[Moon] ) {
    val originDimensionX = getPosAndVelForSystem(this, "x")
    val originDimensionY = getPosAndVelForSystem(this, "y")
    val originDimensionZ = getPosAndVelForSystem(this, "z")

    def timeStep(n :Int=0): System = {
      for (i <- 0 until n) {
        this.moons.map(moon => moon.applyGravity(moons))
        this.moons.map(moon=> moon.move())
      }
      this
    }

    def getMoon(name:String) = {
      this.moons.find(moon => moon.name == name)
    }

    def getTotalEnergy() :Int = {
      this.moons.map(moon => moon.getPotentialEnergy()*moon.getKineticEnergy()).sum
    }


    def repeatsDimensionAfter(limit: Int, stepSize:Int = 1, startCount:Int = 0):(Int,Int,Int) = {
    var x = -1
    var y = -1
    var z = -1
    var counter = startCount
//      if (counter >= limit) {
//        return (-1,-1,-1)
//      }
      for (i <- 0 until limit) {
        counter = counter + stepSize
        this.timeStep(stepSize)
        val dimensionX = getPosAndVelForSystem(this,"x")
        val dimensionY = getPosAndVelForSystem(this,"y")
        val dimensionZ = getPosAndVelForSystem(this,"z")
        (dimensionX, dimensionY, dimensionZ) match {
          case (this.originDimensionX, _, _) => {
//            167624
            if (x == -1) {
              x = counter
            }
            if (x > 0 && y > 0 && z > 0 ) {
              (x,y,z)
            }
//            else {
//              repeatsDimensionAfter(limit, counter+1, stepSize)
//            }
          }
          case (_, this.originDimensionY, _) => {
//            231614
            if (y == -1) {
              y = counter
            }
            if (x > 0 && y > 0 && z > 0 ) {
              (x,y,z)
            }
//            else {
//              repeatsDimensionAfter(limit, counter+1, stepSize)
//            }
          }
          case (_, _, this.originDimensionZ) => {
//            96236
            if (z == -1) {
              z = counter
            }
            if (x > 0 && y > 0 && z > 0 ) {
              (x,y,z)
            }
//            else {
//              repeatsDimensionAfter(limit, counter+1,stepSize)
//            }
          }
          case _ => {
//            repeatsDimensionAfter(limit, counter+1,stepSize)
            println(i)
          }
        }
      }
      (-1,-1,-1)

    }

    def sharedCycle(a:Long, b:Long, c:Long) : Long = {
      val sorted = Array(a,b,c).sorted

      def isCommonInCycle(a:Long, b:Long, cycle:Long):Long ={
        if (cycle % a == 0 && cycle % b == 0) {
          cycle
        } else {
          isCommonInCycle(a, b, cycle+sorted(2))
        }

      }
      isCommonInCycle(sorted(0), sorted(1), 2*sorted(2))
    }

    def getPosAndVelForSystem(system: System, dim: String) : Dimension = {
      val ((p1, v1), (p2, v2), (p3, v3), (p4, v4)) = (system.moons(0).getPosAndVelForDim(dim),
        system.moons(1).getPosAndVelForDim(dim),
        system.moons(2).getPosAndVelForDim(dim),
        system.moons(3).getPosAndVelForDim(dim),
      )
      Dimension(p1,v1,p2,v2,p3,v3,p4,v4)
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

    def move(): Moon = {
      this.position = Position(this.position.x + this.velocity.x, this.position.y + this.velocity.y, this.position.z + this.velocity.z)
      this
    }

    def speedUp(add: Velocity) :Unit = {
      this.velocity = Velocity(this.velocity.x + add.x, this.velocity.y + add.y, this.velocity.z + add.z)
    }

    def getPotentialEnergy():Int = {
      this.position.x.abs + this.position.y.abs + this.position.z.abs
    }

    def getKineticEnergy() :Int = {
      this.velocity.x.abs + this.velocity.y.abs + this.velocity.z.abs
    }

    def getPosAndVelForDim(dim:String) : (Int, Int) = {
      dim match {
        case "x" => (this.position.x, this.velocity.x)
        case "y" => (this.position.y, this.velocity.y)
        case "z" => (this.position.z, this.velocity.z)
      }
    }
  }


  case class Position(x:Int, y:Int, z:Int) {
  }

  case class Velocity(x:Int,y:Int, z:Int) {
  }

  case class Dimension(p1:Int, v1:Int, p2:Int, v2:Int, p3:Int, v3:Int, p4:Int, v4:Int)
}
