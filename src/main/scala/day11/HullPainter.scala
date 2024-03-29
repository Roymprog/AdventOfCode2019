package day11

import day11.HullPainter.Panel.Coordinate
import day2.Intcode.Opcode
import day9.SensorBoost
import day9.SensorBoost.RelativeBaseOpcode

object HullPainter {
  val filename = "src/resources/day11.txt"
  val longs = SensorBoost.getLongs(filename)

  def run1() = {
    val (robot, panels) = parseGrid(".....\n.....\n..^..\n.....\n.....")
    val grid = new Grid(robot, panels)
    val opcode = new RelativeBaseOpcode(longs)
    while(opcode.getOpcode() != Opcode(99)) {
      opcode.input = grid.getInput()
      val first = getOutput(opcode)
      val second = getOutput(opcode)
      if ((first >= 0) && (second >= 0) ) {
        grid.processInstructions(first,second)
      }
    }
    println(s"Painted tiles is: ${grid.countPaintedPanels()}")
  }

  def run2() = {
    val (robot, panels) = parseGrid(".....\n.....\n..^..\n.....\n.....", 1)
    val grid = new Grid(robot, panels)
    val opcode = new RelativeBaseOpcode(longs)
    while(opcode.getOpcode() != Opcode(99)) {
      opcode.input = grid.getInput()
      val first = getOutput(opcode)
      val second = getOutput(opcode)
      if ((first >= 0) && (second >= 0) ) {
        grid.processInstructions(first,second)
      }
    }
    val minx = grid.getPaintedPanels().foldLeft(0)((min, panel) => if (panel.coordinate.x < min) panel.coordinate.x else min  )
    val miny = grid.getPaintedPanels().foldLeft(0)((min, panel) => if (panel.coordinate.y < min) panel.coordinate.y else min  )
    val maxx = grid.getPaintedPanels().foldLeft(0)((max, panel) => if (panel.coordinate.x > max) panel.coordinate.x else max  )
    val maxy = grid.getPaintedPanels().foldLeft(0)((max, panel) => if (panel.coordinate.y > max) panel.coordinate.y else max  )
    val paintedPanels = grid.getPaintedPanels()
    for(y <- miny to maxy; x <- minx to maxx ) {
      val panel = paintedPanels.find(panel => panel.coordinate == Coordinate(x,y))
      paintedPanels.find(panel => panel.coordinate == Coordinate(x,y)) match {
        case None => print(" ")
        case _  => if (panel.get.color== 0) print(" ") else print("8")
      }
      if (x == maxx) {
        println("")
      }
    }
  }

  def getOutput(opcode: RelativeBaseOpcode) :Int = {
    var output :Option[Long]= None
    while(output == None) {
      if (opcode.getOpcode() == Opcode(99)) {
        return -1
      }
      opcode.executeLongCode()
      output = opcode.getOutput()
    }
    output.get.toInt
  }

  def findRobot(lines: Array[String]):Robot = {
    for (y <- 0 until lines.length) {
      for (x <- 0 until lines(y).length ) {
        if (lines(y)(x) == '^') {
          return new Robot(x,y, '^')
        }
      }
    }
    new Robot(0,0, '^')
  }

  def parseGrid(grid: String, robotColor:Int = 0) : (Robot, IndexedSeq[Panel]) = {
    def parseLine(s:String, lineNr:Int) :IndexedSeq[Panel] = {
      def getColor(c:Char):Int = if (c == '#') 1 else if (c == '^') robotColor else 0
      for (i <- 0 until s.length)
        yield new Panel(i, lineNr, getColor(s(i)))
    }
    val lines = grid.split("\n")
    val panels = (for(j <- 0 until lines.length)
      yield parseLine(lines(j), j)).flatten
    (findRobot(lines), panels)
  }

  class Grid(val robot: Robot, var panels: IndexedSeq[Panel]) {
    def getInput() :Int = {
      val panel = panels.find(panel => panel.coordinate == robot.coordinate)
      panel match {
        case None => 0
        case _ => panel.get.color
      }
    }

    def getPaintedPanels() :IndexedSeq[Panel] = {
      panels.filter(panel => panel.painted)
    }

    def countPaintedPanels():Int = {
      panels.count(panel => panel.painted)
    }

    def processInstructions(first:Int,second:Int) :Unit = {
      paintPanel(first)
      robot.move(second)
    }

    private def paintPanel(color:Int) :Unit = {
      val panel = panels.find(panel => panel.coordinate == robot.coordinate)
      panel match {
        case None => {
          val newPanel = new Panel(robot.coordinate.x,robot.coordinate.y,color)
          newPanel.painted = true
          panels = panels.appended(newPanel)
        }
        case _ => {
          panel.get.painted = true
          panel.get.color = color
        }
      }
    }
  }

  class Robot(var x:Int, var y:Int, var direction:Char) {
    def coordinate = Coordinate(x,y)

    private def turnLeft(): Char = {
      direction match {
        case '^' => direction = '<'
        case '<' => direction = 'v'
        case 'v' => direction = '>'
        case '>' => direction = '^'
        case _ => throw new RuntimeException("Direction is not allowed")
      }
      direction
    }

    private def turnRight(): Char = {
      direction match {
        case '^' => direction = '>'
        case '<' => direction = '^'
        case 'v' => direction = '<'
        case '>' => direction = 'v'
        case _ => throw new RuntimeException("Direction is not allowed")
      }
      direction
    }

    private def changeDir(n:Int): Char = {
      n match {
        case 0 => turnLeft()
        case 1 => turnRight()
        case _ => throw new RuntimeException("Direction not allowed")
      }
    }

    def move(n:Int): Unit = {
      changeDir(n)
      direction match {
        case '^' => y = y - 1
        case '<' => x = x - 1
        case 'v' => y = y + 1
        case '>' => x = x + 1
        case _ => throw new RuntimeException("Direction is not allowed")
      }
    }
  }

  class Panel(x:Int,y:Int, var color:Int) {
    val coordinate = Coordinate(x,y)
    var painted = false
  }

  object Panel {
    case class Coordinate(x:Int,y:Int)
  }
}
