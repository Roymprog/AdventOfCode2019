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

  "Asteroid round" should "should return " in {
    val asteroids = new Asteroids("#..\n.#.\n..#")
    asteroids.getBestAsteroid() shouldBe Asteroid(1,1)
    asteroids.getRound() shouldBe IndexedSeq(Asteroid(2,2), Asteroid(0,0))
  }

  "Angle calc" should "return degrees" in {
    Monitoring.angle(Asteroid(0,0), Asteroid(0, -5)).toInt shouldBe 0
    Monitoring.angle(Asteroid(0,0), Asteroid(1,-1)).toInt shouldBe 45
    Monitoring.angle(Asteroid(0,0), Asteroid(1,0)).toInt shouldBe 90
    Monitoring.angle(Asteroid(0,0), Asteroid(1, 1)).toInt shouldBe 135
    Monitoring.angle(Asteroid(0,0), Asteroid(0, 5)).toInt shouldBe 180
    Monitoring.angle(Asteroid(0,0), Asteroid(-1,1)).toInt shouldBe 225
    Monitoring.angle(Asteroid(0,0), Asteroid(-1,0)).toInt shouldBe 270
    Monitoring.angle(Asteroid(0,0), Asteroid(-1,-1)).toInt shouldBe 315
    Monitoring.angle(Asteroid(0,0), Asteroid(0,-1)).toInt shouldBe 0
    Monitoring.angle(Asteroid(8,3), Asteroid(8,1)).toInt shouldBe 0
    Monitoring.angle(Asteroid(8,3), Asteroid(9,0)).toInt shouldBe 18
    Monitoring.angle(Asteroid(8,3), Asteroid(9,1)).toInt shouldBe 26
    Monitoring.angle(Asteroid(8,3), Asteroid(10,0)).toInt shouldBe 33

    Monitoring.angle(Asteroid(11,13), Asteroid(11,12)) shouldBe 0
    Monitoring.angle(Asteroid(11,13), Asteroid(12,1)) shouldBe 5.0
    Monitoring.angle(Asteroid(11,13), Asteroid(12,2)) shouldBe 34
    Monitoring.angle(Asteroid(11,13), Asteroid(12,4)) shouldBe 34
    Monitoring.angle(Asteroid(11,13), Asteroid(12,5)) shouldBe 34
    Monitoring.angle(Asteroid(11,13), Asteroid(12,6)) shouldBe 34
    Monitoring.angle(Asteroid(11,13), Asteroid(12,7)) shouldBe 34
  }

  "Asteroid round" should "be list of" in {
    val asteroids = new Asteroids(".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##")
    val round = asteroids.getRound()
    round.size shouldBe 30
    round.take(5) shouldBe IndexedSeq(Asteroid(8,1), Asteroid(9,0), Asteroid(9,1), Asteroid(10,0), Asteroid(9,2))
  }

  "Asteroid (11,13)" should "have 200th vaporized asteroid be (8,2)" in {
    var asteroids = new Asteroids(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
    asteroids.getBestAsteroid() shouldBe Asteroid(11,13)
    asteroids.getNthVaporized(200) shouldBe 802
    asteroids = new Asteroids(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
    asteroids.getNthVaporized(299) shouldBe 1101
  }
}
