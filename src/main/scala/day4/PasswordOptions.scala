package day4

class PasswordOptions(input: String = "178416-676461") {
  val (lower, upper) = parseinput(input)

  def run() = {
    val countIter = for(i <- lower until upper; if isPasswordOption(i))
      yield i

    for (i <- countIter)
      println(i)

    print("Amount of password options is: ", countIter.size)
  }

  def parseinput(input: String) : (Int, Int) = {
    val splitInput = input.split("-")
    (splitInput(0).toInt, splitInput(1).toInt)
  }

  def isPasswordOption(candidate:Int) :Boolean = {
    if (candidate >= lower && candidate <= upper &&
        hasDoubleDigit(candidate) &&
        !hasDecreasingPairs(candidate)
    ) true else false
  }

  def hasDoubleDigit(int:Int) : Boolean = {
    if (int < 11) {
      false
    }
    // Comment out below 3 else clauses for answer to a
    else if (int % 111111 == 0) {
      false
    }
    else if (int % 10000 % 1111 == 0) {
      hasDoubleDigit(int / 10000)
    } else if(int % 1000 % 111 == 0 ) {
      hasDoubleDigit(int / 1000)
    }
    else if(int % 100 % 11 == 0) {
      true
    } else {
      hasDoubleDigit(int/10)
    }
  }

  def hasDecreasingPairs(int:Int): Boolean = {
    if (int < 10) {
      return false
    }

    def splitInt(int: Int) : (Int, Int) = {
      (int % 100 / 10, int % 10)
    }

    val (first, second) = splitInt(int)

    if (first > second) {
      true
    } else {
      hasDecreasingPairs(int / 10)
    }
  }
}
