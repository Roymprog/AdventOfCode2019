package day11

import day11.HullPainter.Grid
import day11.HullPainter.Panel.Coordinate
import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers

class HullPainterTest extends FlatSpec with Matchers{
  "Parse grid" should "return list of 9 panels with robot in the middle" in {
    val (robot, panels) = HullPainter.parseGrid("...\n.^.\n...")
    panels.map(panel => (panel.coordinate, panel.color)) shouldBe
      IndexedSeq((Coordinate(0,0),0),(Coordinate(1,0),0),(Coordinate(2,0),0)
                ,(Coordinate(0,1),0),(Coordinate(1,1),0),(Coordinate(2,1),0)
                ,(Coordinate(0,2),0),(Coordinate(1,2),0),(Coordinate(2,2),0))
    robot.coordinate shouldBe Coordinate(1,1)
  }

  "Parse grid" should "return have robot in middle" in {
    val (robot, _) = HullPainter.parseGrid(".....\n.....\n..^..\n.....\n.....")
    robot.coordinate shouldBe Coordinate(2,2)
  }

  "Moving robot over grid" should "paint 6 tiles" in {
    val (robot, panels) = HullPainter.parseGrid(".....\n.....\n..^..\n.....\n.....")
    val grid = new Grid(robot,panels)
    grid.processInstructions(1,0)
    grid.processInstructions(0,0)
    grid.processInstructions(1,0)
    grid.processInstructions(1,0)
    grid.getInput() shouldBe 1
    grid.processInstructions(0,1)
    grid.processInstructions(1,0)
    grid.processInstructions(1,0)
    grid.countPaintedPanels() shouldBe 6
    grid.robot.direction shouldBe '<'
    grid.robot.coordinate shouldBe Coordinate(2,1)
  }
}
