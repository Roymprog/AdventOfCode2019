package day5

import day5.ExtendedIntcode.ExtendedCode
import day2.Intcode
import day2.Intcode.{Code, Opcode}
import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers

class ExtendedCodeSpec extends FlatSpec with Matchers{
  "Extended code" should "parse 1002" in {
    val code = new ExtendedCode(Array(1002))

    code.getOpcode() shouldBe Opcode(2)
    code.C shouldBe 0
    code.B shouldBe 1
    code.A shouldBe 0

    val code2 = new ExtendedCode(Array(11101))

    code2.getOpcode() shouldBe Opcode(1)
    code2.C shouldBe 1
    code2.B shouldBe 1
    code2.A shouldBe 1

    val code3 = new ExtendedCode(Array(1101))

    code3.getOpcode() shouldBe Opcode(1)
    code3.C shouldBe 1
    code3.B shouldBe 1
    code3.A shouldBe 0

    val code4 = new ExtendedCode(Array(1))

    code4.getOpcode() shouldBe Opcode(1)
    code4.C shouldBe 0
    code4.B shouldBe 0
    code4.A shouldBe 0

    val code5 = new ExtendedCode(Array(1102))

    code5.getOpcode() shouldBe Opcode(2)
    code5.C shouldBe 1
    code5.B shouldBe 1
    code5.A shouldBe 0
  }

  "Running Code" should "of old opcode should still work" in {
    val array = Array(2,3,0,3,99)
    val code = new ExtendedCode(array)
    code.executeCode()
    array shouldBe Array(2,3,0,6,99)
  }

  "Running Code" should "return 99 at last but one position" in {
    val array = Array(1101,100,-1,4,0)
    val code = new ExtendedCode(array)
    code.executeCode()
    array shouldBe Array(1101,100,-1,4,99)
  }

  "Executing code 3" should "replace its input in location 5" in {
    val array = Array(3,5,99,4,99,13798)
    val code = new ExtendedCode(array)
    code.executeCode()
    array shouldBe Array(3,5,99,4,99,1)
  }

  "Executing code 2" should "multiple 3 and 133 in place and store in index 4" in {
    val array = Array(1102,3,133,4,151,99)
    val code = new ExtendedCode(array)
    code.executeCode()
    array shouldBe Array(1102,3,133,4,399,99)
  }

  "Executing code 2" should "multiply numbers at positions 4 and 5 and store in index 4" in {
    val array = Array(2,4,5,4,133,3)
    val code = new ExtendedCode(array)
    code.executeCode()
    array shouldBe Array(2,4,5,4,399,3)
  }

  "Position Opcode 8 test" should "output 1" in {
    val array = Array(3,9,8,9,10,9,4,9,99,-1,8)
    val code = new ExtendedCode(array, 8)
    code.executeCode()
  }

  "Position Opcode 8 test" should "output 0" in {
    val array = Array(3,9,8,9,10,9,4,9,99,-1,8)
    val code = new ExtendedCode(array, 0)
    code.executeCode()
  }

  "Immediate Opcode 8 test" should "output 1" in {
    val array = Array(3,3,1108,-1,8,3,4,3,99)
    val code = new ExtendedCode(array, 8)
    code.executeCode()
  }

  "Immediate Opcode 7 test" should "output 1" in {
    val array = Array(3,3,1107,-1,8,3,4,3,99)
    val code = new ExtendedCode(array, 8)
    code.executeCode()
  }

  "Position Opcode 7 test" should "output 1" in {
    val array = Array(3,9,8,9,10,9,4,9,99,-1,8)
    val code = new ExtendedCode(array, 8)
    code.executeCode()
  }

  "Position Opcode 7 test" should "output 0" in {
    val array = Array(3,9,8,9,10,9,4,9,99,-1,8)
    val code = new ExtendedCode(array, 0)
    code.executeCode()
  }

  "Positional Opcode 5 test" should "output 1" in {
    val array = Array(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    val code = new ExtendedCode(array, 10)
    code.executeCode()
  }

  "Positional Opcode 5 test" should "output 0" in {
    val array = Array(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    val code = new ExtendedCode(array, 0)
    code.executeCode()
  }

  "Immediate Opcode 5 test" should "output 0" in {
    val array = Array(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    val code = new ExtendedCode(array, 0)
    code.executeCode()
  }

  "Immediate Opcode 5 test" should "output 1" in {
    val array = Array(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    val code = new ExtendedCode(array, 10)
    code.executeCode()
  }
}
