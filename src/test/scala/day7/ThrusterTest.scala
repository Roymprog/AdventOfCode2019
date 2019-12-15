package day7

import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers
import day7.ThrusterOpcode
import day7.ThrusterOpcode.Amplifier

class ThrusterTest extends FlatSpec with Matchers {
  "Amplifiers" should "run in sequence" in {
    val operators = Array(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
    ThrusterOpcode.runSequence(4,3,2,1,0, operators) shouldBe 43210
  }

  "Amplifiers" should "run in sequence2" in {
    val operators = Array(3,23,3,24,1002,24,10,24,1002,23,-1,23,
      101,5,23,23,1,24,23,23,4,23,99,0,0)
    ThrusterOpcode.runSequence(0,1,2,3,4, operators) shouldBe 54321
  }

  "Amplifiers" should "run in sequence3" in {
    val operators = Array(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
      1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
    ThrusterOpcode.runSequence(1,0,4,3,2, operators) shouldBe 65210
  }

  "Amplifier" should "return input in correct order" in {
    val operators = Array(3,1,3,3,4,5,99,5)
    val amplifier = new Amplifier(operators, 89)
    amplifier.applyInput(98) shouldBe 5
    operators shouldBe Array(3,89,3,98,4,5,99,5)
  }

  "Amplifier" should "perform a single run" in {
    val operators = Array(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
    val amplifier = new Amplifier(operators, 4)
    amplifier.applyInput(0) shouldBe 4
    operators shouldBe Array(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,4,0)
  }

  "Quintuples" should "be 120 uniques" in {
    val quintuples =ThrusterOpcode.getUniqueQuintuples().to(List)
    quintuples.size shouldBe 120
  }

  "Feedback loop" should "feed output back" in {
    val operators = Array(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
      27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
    ThrusterOpcode.runSequence(9,8,7,6,5, operators) shouldBe 139629729
  }

  "Feedback loop" should "feed output back2" in {
    val operators = Array(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
      -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
      53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)
    ThrusterOpcode.runSequence(9,7,8,5,6, operators) shouldBe 18216
  }
}
