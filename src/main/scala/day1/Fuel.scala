package day1

import scala.annotation.tailrec
import scala.io.Source

object Fuel {
  def run(): Unit = {
    val filename = "src/resources/day1.txt"
    var massFuel = 0
    for (line <- Source.fromFile(filename).getLines) {
      massFuel = massFuel + extraFuel(line.toInt)
    }

    println(massFuel)
  }

  def calcFuel(mass: Int): Int = {
    mass / 3 - 2
  }

  @tailrec
  def extraFuel(fuel: Int, total: Int = 0): Int = {
    val extra = calcFuel(fuel)
    if (extra <= 0) {
      total
    } else {
      extraFuel(extra, total + extra)
    }
  }
}
