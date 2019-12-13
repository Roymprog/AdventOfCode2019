package day2

import Intcode.Code
import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers

class IntcodeTest extends FlatSpec with Matchers{

  "Running Code" should "return 3500" in {
    val array = Array(1,9,10,3,2,3,11,0,99,30,40,50)
    val code = new Code(array)
    code.executeCode()
    array shouldBe Array(3500,9,10,70,2,3,11,0,99,30,40,50)
  }

  "Running Code" should "return 2" in {
    val array = Array(1,0,0,0,99)
    val code = new Code(array)
    code.executeCode() shouldBe 2
  }

  "Running Code" should "return 6 at last but one position" in {
    val array = Array(2,3,0,3,99)
    val code = new Code(array)
    code.executeCode()
    array shouldBe Array(2,3,0,6,99)
  }

  "Running Code" should "return 9801 in last position" in {
    val array = Array(2,4,4,5,99,0)
    val code = new Code(array)
    code.executeCode()
    array shouldBe Array(2,4,4,5,99,9801)
  }

  "Running Code" should "return 30 in first position" in {
    val array = Array(1,1,1,4,99,5,6,0,99)
    val code = new Code(array)
    code.executeCode()
    array shouldBe Array(30,1,1,4,2,5,6,0,99)
  }
}
