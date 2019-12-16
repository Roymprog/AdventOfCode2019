package day8

import day8.Images.Image
import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers

class ImagesTest extends FlatSpec with Matchers{
  "Small images" should "parse into layers" in {
    val image = new Image(List(1,2,3,4,5,6,7,8,9,0,1,2), 3, 2)
    image.getLayerCount() shouldBe 2

    image.getLayer(0).getNumberOf(5) shouldBe 1
    image.getLayer(1).getNumberOf(9) shouldBe 1

    val layer2 = image.getLayerWithFewest(8)
    layer2.getNumberOf(5) shouldBe image.getLayer(0).getNumberOf(5)
    val layer3 = image.getLayerWithFewest(4)
    layer3.getNumberOf(9) shouldBe image.getLayer(1).getNumberOf(9)
  }
}
