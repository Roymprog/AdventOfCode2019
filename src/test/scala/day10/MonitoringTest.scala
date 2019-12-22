package day10

import day10.Monitoring
import day10.Monitoring.{Asteroid, Asteroids}
import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers

class MonitoringTest extends FlatSpec with Matchers{
  "Asteroid count" should "be 10" in {
    val asteroids = new Asteroids(".#..#\n.....\n#####\n....#\n...##")
    val asters = asteroids.asteroids
    asters.size shouldBe 10
    true shouldBe asters.contains(Asteroid(1,0))
    true shouldBe asters.contains(Asteroid(4,4))
  }

  "Asteroid (3,4)" should "have most visible planets" in {
    val asteroids = new Asteroids(".#..#\n.....\n#####\n....#\n...##")
    val asters = asteroids.asteroids
    asters.size shouldBe 10
    val list = asteroids.asteroidsVisible(asters)
    list.diff(List((Asteroid(4,2),5),(Asteroid(0,2),6),(Asteroid(1,2),7),(Asteroid(2,2),7),(Asteroid(3,2),7)
    ,(Asteroid(4,3),7), (Asteroid(4,4),7), (Asteroid(1,0),7), (Asteroid(4,0),7), (Asteroid(3,4),8))).size shouldBe 0
    asteroids.getBestAsteroid() shouldBe Asteroid(3,4)
  }

  "Asteroid (5,8)" should "have most visible planets" in {
    val asteroids = new Asteroids("......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####")
    asteroids.getBestAsteroid() shouldBe Asteroid(5,8)
    asteroids.getMostAsteroidsVisible() shouldBe 33
  }

  "Asteroid (1,2)" should "have most visible planets" in {
    val asteroids = new Asteroids("#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.")
    asteroids.getBestAsteroid() shouldBe Asteroid(1,2)
    asteroids.getMostAsteroidsVisible() shouldBe 35
  }

  "Asteroid (6,3)" should "have most visible planets" in {
    val asteroids = new Asteroids(".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..")
    asteroids.getBestAsteroid() shouldBe Asteroid(6,3)
    asteroids.getMostAsteroidsVisible() shouldBe 41
  }

  "Asteroid (11,13)" should "have most visible planets" in {
    val asteroids = new Asteroids(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
    asteroids.getBestAsteroid() shouldBe Asteroid(11,13)
    asteroids.getMostAsteroidsVisible() shouldBe 210
  }

  "Asteroid rotatoin" should "return correct path order" in {
    val asteroids = new Asteroids(".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....X...###..\n..#.#.....#....##")
    asteroids.getBestAsteroid() shouldBe Asteroid(8,3)
    val angleSorted = asteroids.angleSorted(asteroids.asteroids)
    angleSorted shouldBe IndexedSeq(Asteroid(1,0), Asteroid(1,-3),Asteroid(1,-2))
  }

  "Angle calc" should "return degrees" in {
    Monitoring.angle(Asteroid(0,0), Asteroid(0, -5)) shouldBe 0
    Monitoring.angle(Asteroid(0,0), Asteroid(1,-1)) shouldBe 45
    Monitoring.angle(Asteroid(0,0), Asteroid(1,0)) shouldBe 90
    Monitoring.angle(Asteroid(0,0), Asteroid(1, 1)) shouldBe 135
    Monitoring.angle(Asteroid(0,0), Asteroid(0, 5)) shouldBe 180
    Monitoring.angle(Asteroid(0,0), Asteroid(-1,1)) shouldBe 225
    Monitoring.angle(Asteroid(0,0), Asteroid(-1,0)) shouldBe 270
    Monitoring.angle(Asteroid(0,0), Asteroid(-1,-1)) shouldBe 315
    Monitoring.angle(Asteroid(0,0), Asteroid(0,-1)) shouldBe 0
    Monitoring.angle(Asteroid(8,3), Asteroid(8,1)) shouldBe 0
  }
}
