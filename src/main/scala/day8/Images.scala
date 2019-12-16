package day8

import day3.Wires.filename

import scala.io.Source

object Images {
  val filename = "src/resources/day8.txt"

  def run1() = {
    val input = Source.fromFile(filename).mkString.stripLineEnd
    val intInput = (for (i <- input)
      yield (i.toInt - 48)).toList

    val image = new Image(intInput, 25, 6)

    val layer = image.getLayerWithFewest(0)
    println(s"Layer with least zeros has ${layer.getNumberOf(2)*layer.getNumberOf(1)} number of ones times number of twos ")
  }

  class Image(imageData :List[Int], width: Int, height: Int) {
    private val layers :List[Layer] = parseLayers()

    def getLayerCount() :Int = {
      layers.size
    }

    def getLayer(n:Int) : Layer = {
      layers(n)
    }

    def getLayerWithFewest(n:Int):Layer = {
      layers.sortBy(_.getNumberOf(n)).head
    }

    def getPixel(n:Int) : Int = {
        layers.find(layer => layer.getPixel(n) != 2).get.getPixel(n)
    }

    private def parseLayers(): List[Layer] = {
      if ((imageData.size % width*height) != 0) {
        throw new RuntimeException("Input data does not match given width and height")
      }

      def antiFlatten(integers :List[Int], dim:Int) : List[List[Int]] = {
        if (integers.size == 0) {
          List()
        } else {
          integers.take(dim) :: antiFlatten(integers.drop(dim), dim)
        }
      }

      antiFlatten(imageData, width*height).map(p => new Layer(p, width,height))
    }

    class Layer(pixels: List[Int], width: Int, height: Int) {
      def getPixel(n:Int) :Int = {
        pixels(n)
      }

      def getNumberOf(n:Int):Int = {
        pixels.count(p => p == n)
      }
    }
  }
}
