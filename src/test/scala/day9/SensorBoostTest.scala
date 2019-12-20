package day9

import day9.SensorBoost.RelativeBaseOpcode
import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers

class SensorBoostTest extends FlatSpec with Matchers{
  "Relative base opcode" should "process code 9" in {
    val code = new RelativeBaseOpcode(List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
    code.executeLongCode() shouldBe 99

  }

  "Relative base opcode" should "output big number" in {
    val code = new RelativeBaseOpcode(List(104,1125899906842624L,99))
    code.executeLongCode() shouldBe 1125899906842624L
  }

  "Relative base opcode" should "output big number 2" in {
    val code = new RelativeBaseOpcode(List(1102,34915192,34915192,7,4,7,99,0))
    val output = code.executeLongCode()
    true shouldBe output > 111111111111111L
  }
}
