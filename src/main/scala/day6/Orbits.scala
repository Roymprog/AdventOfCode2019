package day6

import scala.io.Source

object Orbits {
  val filename = "src/resources/day6.txt"

  def run():Unit = {
    val orbits = Source.fromFile(filename).getLines().toList
    val solarSystem = new SolarSystem(orbits)

    println(s"Amount of orbits in solar system are: ${solarSystem.com.orbitCount()}")
  }

  class SolarSystem(orbits: List[String]) {
    val com = new Planet("COM")
    private var planets: List[Planet] = createSolarSystem()

    private def createSolarSystem():List[Planet] = {
      var solarSystem = List(com)
      def addToSolarSystem(planetA:Planet, planetB:Planet) :Unit = {
        var newPlanet = planetB
        if (solarSystem.contains(planetB)) {
          newPlanet = solarSystem.find(planet => planetB.id() == planet.id()).get
        }

        solarSystem.contains(planetA) match {
          case false => {
            solarSystem = planetA :: solarSystem
            planetA.addChild(newPlanet)
          }
          case true => solarSystem.find(planet => planetA.id() == planet.id()).get.addChild(newPlanet)
        }

        solarSystem = newPlanet :: solarSystem
      }

      for ((planetA, planetB) <- orbits.map(parsePlanets))
        addToSolarSystem(planetA, planetB)
      solarSystem
    }

    def parsePlanets(s: String): (Planet, Planet) = {
      val planets = s.split(')')
      (new Planet(planets(0)), new Planet(planets(1)))
    }
  }

  class Planet(id:String) {
    def id():String = id

    var children : List[Planet] = List()

    def addChild(inOrbit: Planet) :Unit = {
      children = inOrbit :: children
    }

//    def pathToPlanet(id: String, route:List[String] = List()) : List[String] = {
//      if (this.id == id) {
//        val newRoute = route.appended(id)
//        newRoute.appendedAll(children.map(child => child.pathToPlanet(id, newRoute)))
//      }
//    }

    def hasChild(id: String) : Boolean = {
      if (this.id == id) {
        true
      } else if (children.map(child => child.hasChild(id)).isEmpty) {
        false
      } else {
        children.map(child => child.hasChild(id)).contains(true)
      }
    }

    def canEqual(a: Any):Boolean = a.isInstanceOf[Planet]

    override def equals(that: Any) :Boolean = {
      that match {
        case that: Planet => {
          that.canEqual(this) &&
            this.id == that.id()
        }
        case _ => false
      }
    }

    def orbitCount(depth: Int = 0): Int = {
      depth + children.map(child => child.orbitCount(depth+1)).sum
    }
  }
}
