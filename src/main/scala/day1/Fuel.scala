package day1

import scala.io.Source

object Fuel {
  def run(): Unit = {
    val filename = "/home/royvs/Programming/advent_of_code_2019/src/resources.txt"
    var massFuel = 0
    for (line <- Source.fromFile(filename).getLines) {
      massFuel = massFuel + extraFuel(line.toInt)
    }

    println(massFuel)
  }

  def calcFuel(mass: Int): Int = {
    mass / 3 - 2
  }

  def extraFuel(fuel: Int, total: Int = 0): Int = {
    val extra = calcFuel(fuel)
    if (extra <= 0) {
      total
    } else {
      extraFuel(extra, total + extra)
    }
  }
}
