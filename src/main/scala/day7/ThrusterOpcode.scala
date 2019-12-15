package day7

import day2.Intcode
import day2.Intcode.Opcode
import day5.ExtendedIntcode.ExtendedCode

object ThrusterOpcode {
  val filename = "src/resources/day7.txt"

  def run1() :Unit = {
    val codeList = Intcode.getIntList(filename)

    val n = 5

    val signals = getUniqueQuintuples(n).to(List)
      .map(quintuple => runSequence(quintuple._1,quintuple._2,quintuple._3,quintuple._4,quintuple._5, codeList))

    val maxThruster = signals.max

    println(s"Maximum thruster signal is ${maxThruster}")
  }

  def run2() :Unit = {
    val codeList = Intcode.getIntList(filename)

    val start = 5
    val n = 10

    val signals = getUniqueQuintuples(n, start).to(List)
      .map(quintuple => runSequence(quintuple._1,quintuple._2,quintuple._3,quintuple._4,quintuple._5, codeList))

    val maxThruster = signals.max

    println(s"Maximum thruster signal with feedback is ${maxThruster}")
  }

  def getUniqueQuintuples(n:Int = 5, start:Int = 0) :IterableOnce[(Int,Int,Int,Int,Int)] = {
    for (a <- start until n;
         b <- start until n;
         c <- start until n;
         d <- start until n;
         e <- start until n; if Set(a,b,c,d,e).size == 5)
      yield (a,b,c,d,e)
  }

  def runSequence(a:Int,b:Int, c:Int,d:Int,e:Int, operators:Array[Int]) :Int = {
    val amplifier1 = new Amplifier(operators.clone(), a)
    val amplifier2 = new Amplifier(operators.clone(), b)
    val amplifier3 = new Amplifier(operators.clone(), c)
    val amplifier4 = new Amplifier(operators.clone(), d)
    val amplifier5 = new Amplifier(operators.clone(), e)
    amplifier1.setNextAmplifier(amplifier2)
    amplifier2.setNextAmplifier(amplifier3)
    amplifier3.setNextAmplifier(amplifier4)
    amplifier4.setNextAmplifier(amplifier5)
    amplifier5.setNextAmplifier(amplifier1)

    val amplifiers = Array(amplifier1, amplifier2, amplifier3, amplifier4, amplifier5)
    var in = 0
    while(amplifier5.getOpcode() != Opcode(99)) {
      in = amplifiers.foldLeft(in)((input, amplifier) => amplifier.applyInput(input))
    }
    in
  }

  class Amplifier(operators:Array[Int], amplifierPhase:Int) extends ExtendedCode(operators, amplifierPhase) {
    private var inputs = List(amplifierPhase)
    private var returnValue = 0
    private var _outputAmplifier :Amplifier= null

    def setNextAmplifier(amplifier: Amplifier):Unit = {
      _outputAmplifier = amplifier
    }

    override def getInputInstruction(): Int = {
      val first = inputs.head
      inputs = inputs.drop(1)
      first
    }

    override def getOutput(): Int = returnValue

    override def executeOpcode4() :Int = {
      returnValue = getFirstInput()
      moveIndex(2)
//      if (amplifierPhase > 4) {
//        _outputAmplifier.applyInput(returnValue)
//      } else {
//      executeCode()
//      }
      returnValue
//      super.executeOpcode4()
    }

//    apply input and execute code
    def applyInput(input: Int) :Int = {
      inputs = inputs.appended(input)
      super.executeCode()
    }
  }
}
