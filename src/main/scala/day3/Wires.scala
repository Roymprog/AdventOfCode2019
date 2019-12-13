package day3

import scala.io.Source

object Wires {
  val filename = "/home/royvs/Programming/advent_of_code_2019/src/resources/day3.txt"

  def run():Unit = {
    val input = Source.fromFile(filename).getLines

    val (wireString1, wireString2) = extractWireInput(input)
    val wire1 = new Wire(wireString1)
    val wire2 = new Wire(wireString2)

    val intersections = wire1.getIntersections(wire2)

    val closest = closestToOrigin(intersections)
    val shortestSignal = shortestSignalPath(intersections, wire1, wire2)

    println("Manhattan distance of point closest to origin is %d", manhattanDistance(closest))
    println("Shortest combined path is %d", summedDistanceToPoint(shortestSignal, wire1, wire2))
  }

  def extractWireInput(input : Iterator[String]) : (Array[String], Array[String]) = {
    (input.next().split(","), input.next().split(","))
  }

  def shortestSignalPath(points :Set[Point], wireA: Wire, wireB: Wire) :Point = {
    def shortestSignal(point1: Point, point2: Point) : Point = {
      if (summedDistanceToPoint(point1, wireA, wireB) < summedDistanceToPoint(point2, wireA, wireB)) point1 else point2
    }

    points reduceLeft shortestSignal
  }


  def summedDistanceToPoint(point: Point, wireA: Wire, wireB: Wire): Int = {
    wireA.stepsToPoint(point) + wireB.stepsToPoint(point)
  }

  def closestToOrigin(points :Set[Point]) :Point = {
    points reduceLeft closestToOrigin
  }

  def closestToOrigin(first: Point, second: Point) :Point = {
    if (manhattanDistance(first) < manhattanDistance(second)) first else second
  }

  def manhattanDistance(point: Point) : Int = {
    point.x.abs + point.y.abs
  }

  class Wire(directions: Array[String]) {
    private var _current = Point(0,0)
    private val _points = setPoints(directions)

    private def setPoints(directions: Array[String]): Set[Point] = {
      directions.flatMap(dir => setPoints(dir)).toSet
    }

    private def setPoints(direction: String) : Set[Point] = {
      val dir = new Direction(direction)
      _current = dir.movePoint(_current)
      dir.getPointsOnPath(_current)
    }

    def getPoints() :Set[Point] = _points

    def getEnd(): Point = _current

    def getIntersections(wire:Wire) :Set[Point] = {
      this.getPoints() intersect wire.getPoints()
    }

    def stepsToPoint(p:Point, start:Point = Point(0,0), total:Int=0) :Int = {
      if (!this.getPoints().contains(p)) {
        throw new Exception("Point is not on path of the wire")
      }

      var total = 0
      var current = Point(0,0)
      for (dir <- this.directions) {
        val d = new Direction(dir)
        val next = d.movePoint(current)
        if (p == next) {
          return total + d.getDistance()
        } else if (d.getPointsOnPath(next).contains(p)) {
          next match {
            case Point(p.x, _) => return total + (current.y.abs - p.y.abs).abs
            case Point(_, p.y) => return total + (current.x.abs - p.x.abs).abs
          }
        } else {
          total = total + d.getDistance()
        }
        current = next
      }
      total
    }
  }

  class Direction(direction: String) {
    val dir = direction.slice(0,1)
    val distance = direction.substring(1)

    def getDirection():String = dir

    def getDistance() :Int = distance.toInt

    def movePoint(point: Point):Point ={
      this.getDirection() match {
        case "U" => Point(point.x, point.y + this.getDistance())
        case "D" => Point(point.x, point.y - this.getDistance())
        case "L" => Point(point.x - this.getDistance(), point.y)
        case "R" => Point(point.x + this.getDistance(), point.y)
      }
    }

    def getPointsOnPath(point: Point): Set[Point] = {
      this.getDirection() match {
        case "U" => (for {y <- point.y - this.getDistance() + 1 until point.y + 1} yield Point(point.x, y)).toSet
        case "D" => (for {y <- point.y until point.y + this.getDistance()} yield Point(point.x, y)).toSet
        case "L" => (for {x <- point.x until point.x + this.getDistance()} yield Point(x, point.y)).toSet
        case "R" => (for {x <- point.x - this.getDistance() + 1 until point.x + 1} yield Point(x, point.y)).toSet
      }
    }
  }

  case class Point(x: Int, y: Int)
}
