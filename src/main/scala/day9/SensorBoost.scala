package day9

import day2.Intcode.{Code, Opcode}
import day5.ExtendedIntcode.ExtendedCode

import scala.io.Source

object SensorBoost {
  val filename = "src/resources/day9.txt"

  def run1() ={
    val fileContents = Source.fromFile(filename).getLines.mkString
    val longs =fileContents.split(",").map(string => string.toLong).toList
    val boost = new RelativeBaseOpcode(longs, 1)

    println(s"Boost code produces: ${boost.executeLongCode()}")
  }

  def run2() ={
    val fileContents = Source.fromFile(filename).getLines.mkString
    val longs =fileContents.split(",").map(string => string.toLong).toList
    val boost = new RelativeBaseOpcode(longs, 2)

    println(s"Boost program distress coordinates: ${boost.executeLongCode()}")
  }

  class RelativeBaseOpcode(operators: List[Long], input: Long = 0) extends Code(operators.map(x => x.toInt).toArray) {
    var A = 0
    var B = 0
    var C = 0
    var DE = 0
    var relativeBase :Long = 0
    var output :Long = 0
    var tupleOperators = operators.zipWithIndex

    def getAtIndex(i:Int) :Long = {
      val found = tupleOperators.find(x => x._2 == i)
      found match {
        case None => 0
        case _ => found.get._1
      }
    }

    def setAtIndex(n:Long, i:Int) :Unit = {
      tupleOperators = (tupleOperators filter { case (_, x) => x != i }).appended((n,i))
    }

    def executeOpcode9(): Unit = {
      relativeBase = relativeBase + getFirst()
      moveIndex(2)
//      executeLongCode()
    }

    def executeLongCode(): Long = {
      var out :Long = 0
      while(out == 0) {
        getOpcode() match {
          case Opcode(1) => applyOperator((a:Long,b:Long) => a+b)
          case Opcode(2) => applyOperator((a:Long,b:Long) => a*b)
          case Opcode(3) => {
            var idx = 0
            if (C == 0) {
              idx = getAtIndex(index+1).toInt
            } else if (C == 1){
              idx = index+1
            } else {
              idx = getAtIndex(index+1).toInt + relativeBase.toInt
            }
            setAtIndex(getInputInstruction(), idx)
            moveIndex(2)
//            executeLongCode()
          }
          case Opcode(4) => executeOpcode4()
          case Opcode(5) => {
            jumpIf(true)
//            executeLongCode()
          }
          case Opcode(6) => {
            jumpIf(false)
//            executeLongCode()
          }
          case Opcode(7) => {
            assignIf(_ < _)
            super.moveIndex()
//            executeLongCode()
          }
          case Opcode(8) => {
            assignIf(_ == _)
            super.moveIndex()
//            executeLongCode()
          }
          case Opcode(9) => executeOpcode9()
          case Opcode(99) => out = getOutput()
          case Opcode(_) => {
            throw new RuntimeException("Unknown opcode")}
        }

      }
      out
    }

    def executeOpcode4():Unit = {
      println(s"Returned: ${getFirst()}")
      output = getFirst()
      moveIndex(2)
//      executeLongCode()
    }

    def getOutput() = output

    def getInputInstruction():Long = this.input

    def jumpIf(condition: Boolean):Unit = {
      if ((getFirst() != 0) == condition) {
        index = getSecond().toInt
      } else {
        moveIndex(3)
      }
    }

    def assignIf(condition: (Long, Long) => Boolean) :Unit = {
      if (condition(getFirst(), getSecond())) {
        store(1)
      } else {
        store(0)
      }
    }

    def positionFirst() : Long = {
      val firstidx = getAtIndex(index + 1).toInt
      getAtIndex(firstidx)
    }

    def positionSecond() : Long = {
      getAtIndex(getAtIndex(index + 2).toInt)
    }

    def getFirst(): Long = {
      conditionalExecution(C, positionFirst, 1)
    }

    def getSecond(): Long = {
      conditionalExecution(B, positionSecond, 2)
    }

    def conditionalExecution(parameter: Long, overwritten:() => Long, offset: Int): Long = {
      parameter match {
        case 0 => overwritten()
        case 1 => getAtIndex(index+offset)
        case 2 => getAtIndex(getAtIndex(index+offset).toInt + relativeBase.toInt)
        case _ => throw new Exception("Mode of $parameter instruction is incorrect")
      }
    }

    override def getOpcode(): Opcode = {
      val instruction = getAtIndex(index).toInt
      A = instruction / 10000
      B = instruction % 10000 / 1000
      C = instruction % 1000 / 100
      DE = instruction % 100
      Opcode(DE)
    }

    def moveIndex(i:Int) {
      index = index + i
    }

    def applyOperator(op : (Long, Long) => Long):Unit = {
      val first = getFirst()
      val second = getSecond()
      val output = op(first, second)
      store(output)
      moveIndex(4)
//      executeLongCode()
    }

    override def getStorageIndex(): Int = {
      if (A == 0) {
        getAtIndex(index+3).toInt
      } else if (A == 1){
        index+3
      } else {
        getAtIndex(index+3).toInt + relativeBase.toInt
      }
    }

    def store(output: Long) :Unit = {
      val idx = getStorageIndex()
      setAtIndex(output, idx)
    }
  }
}
