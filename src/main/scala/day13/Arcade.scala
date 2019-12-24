package day13

import day11.HullPainter.Panel.Coordinate
import day11.HullPainter.getOutput
import day2.Intcode.Opcode
import day9.SensorBoost
import day9.SensorBoost.RelativeBaseOpcode

object Arcade {
  val filename = "src/resources/day13.txt"


  def run1() ={
    val (_, grid) = buildGrid(filename)

    println(s"There are ${grid.countTiles(2)} block tiles on the field")

    grid.printGrid()
  }

  def run2() ={
    var longs = SensorBoost.getLongs(filename)
    longs = longs.patch(0, Seq(2), 1)

    val opcode = new RelativeBaseOpcode(longs)
    val grid = new Grid()
    while(opcode.getOpcode() != Opcode(99)) {
      opcode.input = getInput(grid)
      val (first,second,third) = (getOutput(opcode), getOutput(opcode), getOutput(opcode))
      if (first == -1 && second == 0) {
        println(s"Current player score is: ${third}")
      } else if (third != 1) {
        grid.addTile(first,second,third)
      }
//      println(s"Tiles left: ${grid.countTiles(2)}")
//      if (List(3,4).contains(third)) {
//        grid.printGrid()
//      }
    }
    grid.printGrid()
  }

  def getInput(grid:Grid) :Int = {
    if ((grid.paddle.coordinate.x > grid.ball.coordinate.x) ) {
      -1
    } else if ((grid.paddle.coordinate.x < grid.ball.coordinate.x) ) {
      1
    } else {
      0
    }
  }

  def buildGrid(filename:String, mode:Int = 0) :(List[Long], Grid) = {
    var longs = SensorBoost.getLongs(filename)
    longs = longs.patch(0, Seq(mode), 1)

    val opcode = new RelativeBaseOpcode(longs)
    val grid = new Grid()
    while(opcode.getOpcode() != Opcode(99)) {
      val (first,second,third) = (getOutput(opcode), getOutput(opcode), getOutput(opcode))
      grid.addTile(first,second,third)
    }
    (opcode.operators, grid)
  }

  class Grid() {
    var tiles:IndexedSeq[Tile] = IndexedSeq()
    var paddle = new Tile(0,0,3)
    var ball = new Tile(0,0, 4)

    def addTile(x:Int,y:Int,id:Int) :Unit = {
      val tile = tiles.find(tile => tile.coordinate == Coordinate(x,y))

      val oldTileCount = this.countTiles(2)
      tiles = tiles.filter(tile => tile.coordinate != Coordinate(x,y))
      if (id == 3) {
        paddle = new Tile(x,y,id)
        tiles = tiles.appended(paddle)
      } else if(id ==4) {
        ball = new Tile(x,y,id)
        tiles = tiles.appended(ball)
      } else {
        tiles = tiles.appended(new Tile(x, y, id))
        if(tiles.size == 761) {
          if (tile != None && tile.get.id == 2 && id == 0) {
            println("Replacing tile with id 2")
            println(s"Remaining tiles was:${oldTileCount}. New tile count is: ${this.countTiles(2)}")
          }
        }
      }
    }

    def countTiles(id:Int) :Int = {
      tiles.count(tile => tile.id == id)
    }

    def printGrid() :Unit = {
      val maxx = this.tiles.foldLeft(0)((max, tile) => if (tile.coordinate.x > max) tile.coordinate.x else max  )
      val maxy = this.tiles.foldLeft(0)((max, tile) => if (tile.coordinate.y > max) tile.coordinate.y else max  )
      for(y <- 0 to   maxy; x <- 0 to maxx ) {
        val tile = this.tiles.find(tile => tile.coordinate == Coordinate(x,y))
        tile match {
          case None => print(" ")
          case _  => if (tile.get.id == 0) print(" ") else if(tile.get.id == 3) print("_") else if(tile.get.id == 4) print("O") else print(tile.get.id)
        }
        if (x == maxx) {
          println("")
        }
      }
    }
  }

  class Tile(x:Int, y:Int, var id:Int) {
    def coordinate = Coordinate(x,y)
  }

  case class Coordinate(x:Int, y:Int)
}
