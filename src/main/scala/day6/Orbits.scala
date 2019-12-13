package day6

import scala.io.Source

object Orbits {
  val filename = "src/resources/day6.txt"
  val orbits = Source.fromFile(filename).getLines().toList
  val solarSystem = new SolarSystem(orbits)

  def run1():Unit = {

    println(s"Amount of orbits in solar system are: ${solarSystem.com.orbitCount()}")
  }

  def run2():Unit = {
    println(s"Amount of orbits between us and Santa is: ${solarSystem.com.getPathDiffs("YOU", "SAN")}")
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

    def getPlanet(id: String) :Option[Planet] = {
      if (hasChild(id)) {
        if (this.id == id) {
          Some(this)
        } else if (children.map(child => child.hasChild(id)).isEmpty) {
          None
        } else {
          children.flatMap(child => child.getPlanet(id)).find(planetOption => planetOption != None)
        }
      } else {
        None
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

    def getPathTo(name:String, path : List[String] = List()):Option[List[String]] = {
      if (this.id == name) {
        Some(path)
      }else if(this.children.isEmpty) {
        None
      } else {
        val results = children.map(child => child.getPathTo(name, path.appended(this.id))).flatten
        results.size match {
          case 0 => None
          case _ => Some(results.flatten)
        }
      }
    }

    def getPathDiffs(s1:String, s2:String):Int = {
      val path1 = this.getPathTo(s1).get
      val path2 = this.getPathTo(s2).get

      (path1.toSet diff path2.toSet).size + (path2.toSet diff path1.toSet).size
    }
  }
}
