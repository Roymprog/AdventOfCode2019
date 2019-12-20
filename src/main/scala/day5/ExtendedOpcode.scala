package day5

import day2.Intcode
import day2.Intcode.{Code, Opcode}

object ExtendedIntcode {
  val filename = "src/resources/day5.txt"

  def run(): Unit = {
    val codeList = Intcode.getIntList(filename)
    val code = new ExtendedCode(codeList, 5)
    code.executeCode()
  }

  class ExtendedCode(operators: Array[Int], input: Int = 0) extends Code(operators) {
    var A = 0
    var B = 0
    var C = 0
    var DE = 0
    protected var relativeBase = 0

    def executeOpcode9(): Int = {
      moveIndex(2)
      executeCode()
    }

    override def executeCode(): Int = {
      getOpcode() match {
        case Opcode(1) => applyOperator(add)
        case Opcode(2) => applyOperator(multiply)
        case Opcode(3) => {

           operators(operators(index+1)) = getInputInstruction()
           moveIndex(2)
           executeCode()
        }
        case Opcode(4) => executeOpcode4()
        case Opcode(5) => {
          jumpIf(true)
          executeCode()
        }
        case Opcode(6) => {
          jumpIf(false)
          executeCode()
        }
        case Opcode(7) => {
          assignIf(_ < _)
          super.moveIndex()
          executeCode()
        }
        case Opcode(8) => {
          assignIf(_ == _)
          super.moveIndex()
          executeCode()
        }
        case Opcode(9) => executeOpcode9()
        case Opcode(99) => getOutput()
      }
    }

    def executeOpcode4():Int = {
      println(getFirstInput())
      moveIndex(2)
      executeCode()
    }

    def getOutput() = operators(0)

    def getInputInstruction():Int = this.input

    def jumpIf(condition: Boolean):Unit = {
      if ((getFirstInput() != 0) == condition) {
        index = getSecondInput()
      } else {
        moveIndex(3)
      }
    }

    def assignIf(condition: (Int, Int) => Boolean) :Unit = {
      if (condition(getFirstInput(), getSecondInput())) {
        operators(getStorageIndex()) = 1
      } else {
        operators(getStorageIndex()) = 0
      }
    }

    override def getFirstInput(): Int = {
      conditionalExecution(C, super.getFirstInput, 1)
    }

    override def getSecondInput(): Int = {
      conditionalExecution(B, super.getSecondInput, 2)
    }

    override def getStorageIndex(): Int = {
      A match {
        case 0 => super.getStorageIndex()
        case 1 => index + 3
        case 2 => super.getStorageIndex() + relativeBase
        case _ => throw new Exception("Mode of $parameter instruction is incorrect")
      }
    }

    def conditionalExecution(parameter: Int, overwritten:() => Int, offset: Int): Int = {
      parameter match {
        case 0 => overwritten()
        case 1 => operators(index + offset)
        case 2 => operators(operators(index + offset) + relativeBase)
        case _ => throw new Exception("Mode of $parameter instruction is incorrect")
      }
    }

    override def getOpcode(): Opcode = {
      val instruction = operators(index)
      A = instruction / 10000
      B = instruction % 10000 / 1000
      C = instruction % 1000 / 100
      DE = instruction % 100
      Opcode(DE)
    }

    def moveIndex(i:Int) {
      index = index + i
    }
  }
}
