package day2

import scala.io.Source

object Intcode {
  val filename = "src/resources/day2.txt"

  def run(): Unit = {
    for (i <- 0 until 100;
         j <- 0 until 100 if runCode(i, j) == 19690720)
     println("100*noun(%d) + verb(%d) = %d", i,j,100*i+j)
  }

  def getIntList(filename: String) : Array[Int] = {
    val fileContents = Source.fromFile(filename).getLines.mkString
    fileContents.split(",").map(string => string.toInt)
  }

  def runCode(noun : Int, verb: Int) : Int = {

    val codeList = getIntList(filename)
    // Replace position 1 with value 12
    codeList(1) = noun
    // Replace position 2 with value 2
    codeList(2) = verb
    val code = new Code(codeList)
    code.executeCode()
  }

  class Code(operators: Array[Int]) {
    var index = 0

    def getOpcode(): Opcode = Opcode(operators(index))

    def getFirstInput(): Int = operators(operators(index + 1))

    def getSecondInput(): Int = operators(operators(index + 2))

    def getStorageIndex() :Int = operators(index + 3)

    def storeOutput(output: Int) :Unit = operators(getStorageIndex()) = output

    def moveIndex() :Unit = {
      index = index + 4
    }

    def executeCode(): Int = {
      getOpcode() match {
        case Opcode(1) => applyOperator(add)
        case Opcode(2) => applyOperator(multiply)
        case Opcode(99) => operators(0)
        case _ => throw new Exception("Unknown opcode encountered")
      }
    }

    def add(x: Int, y: Int) : Int = {
      x + y
    }

    def multiply(x: Int, y: Int) : Int = {
      x * y
    }

    def applyOperator(op : (Int, Int) => Int) = {
      val output = op(getFirstInput(), getSecondInput())
      storeOutput(output)
      moveIndex()
      executeCode()
    }

  }

  case class Opcode(code: Int)
}
