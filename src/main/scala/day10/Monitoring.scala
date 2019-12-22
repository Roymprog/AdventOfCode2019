package day10

import day10.Monitoring.Asteroids
import day8.Images.filename
import Math.{atan,round}
import scala.io.Source

object Monitoring {
  val filename = "src/resources/day10.txt"
  val input = Source.fromFile(filename).mkString

  def run1() = {
    val asteroids = new Asteroids(input)
    println(s"Most asteroid detectable is: ${asteroids.getMostAsteroidsVisible()}")
  }

  // Y-axis is inverted in given grid, account for this
  def angle(origin: Asteroid, a:Asteroid) :Int = {
    val x = -(a.x.floatValue() - origin.x.floatValue())
    val y = a.y.floatValue() - origin.y.floatValue()
    var bonus = 0
    if (y >= 0 ) {
      bonus = bonus + 180
    }
//    if (x < 0) {
//      bonus = bonus + 180
//    }
//    else if (y > 0) {
//      bonus = 90
//    }
//    if ( x<0 ) {
//      bonus = 180
//    }
    var angle = (atan(x/y).toDegrees.toInt +bonus)
    if (angle < 0) {
        angle + 360
    } else {
      angle
    }
  }

  class Asteroids(input:String) {
    val (width, height, asteroids) = parseInput(input)
    val bestAsteroid = getBestAsteroid()


    def angleSorted(asteroids: IndexedSeq[Asteroid]): IndexedSeq[(Int, Asteroid)] = {
      asteroids.filter(asteroid => asteroid != bestAsteroid)
               .map(asteroid => (angle(bestAsteroid, asteroid), asteroid))
               .sortBy(_._1)
    }

    def getBestAsteroid() :Asteroid = {
      asteroidsVisible(asteroids).last._1
    }

    def getMostAsteroidsVisible() :Int = {
      asteroidsVisible(asteroids).last._2
    }

     def parseInput(input:String): (Int,Int, IndexedSeq[Asteroid]) = {
      val lines = input.split("\n")
      val height = lines.length
      val width = lines(0).length
      val asteroids = (for(j <- 0 until lines.length)
                        yield parseLine(lines(j), j)).flatten
      (width,height,asteroids)
    }

    private def parseLine(s:String, lineNr:Int) :IndexedSeq[Asteroid] = {
      for (i <- 0 until s.length if s(i) == '#')
        yield Asteroid(i, lineNr)
    }

    def asteroidsVisible(asteroids: IndexedSeq[Asteroid]) : IndexedSeq[(Asteroid, Int)] = {
      asteroids.map(asteroid => (asteroid, findVisible(asteroids, asteroid))).sortBy(_._2)
    }

    def findVisible(asteroids:IndexedSeq[Asteroid], asteroid: Asteroid): Int = {
      def dropInPath(base: Asteroid, next:Asteroid): IndexedSeq[Asteroid] = {
        var list: IndexedSeq[Asteroid]= IndexedSeq()
        var n = 1
        val p = path(base, next)
        while(((next.x + n*p.x) < width) && ((next.x + n*p.x) >= 0) &&
          ((next.y + n*p.y) < height) && ((next.y + n*p.y) >= 0)) {
          val newAsteroid = Asteroid(next.x + n*p.x, next.y + n*p.y)
          if (asteroids.contains(newAsteroid)) {
            list = list.appended(newAsteroid)
          }
          n = n + 1
        }
        list
      }

      val allButSelf = asteroids.filter(a => a != asteroid)
      val filtered = allButSelf
        .flatMap(a => dropInPath(asteroid, a)).toSet
      allButSelf.toSet.diff(filtered).size
    }



    def path(a:Asteroid, b:Asteroid) : Asteroid = {
      var i = 2
      val diffx = (b.x-a.x)
      val diffy = (b.y-a.y)
      var trajectory = Asteroid((b.x-a.x), (b.y-a.y))
      // Find smalles straight path between a and b
      while((i <= diffx.abs) || (i <= diffy.abs)) {
        if ((diffx % i == 0) && (diffy % i == 0)) {
          trajectory =  Asteroid(diffx / i, diffy / i)
        }
        i = i + 1
      }
      trajectory
    }
  }

  case class Asteroid(x:Int, y:Int)
}
