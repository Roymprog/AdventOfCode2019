package day4
import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class PasswordOptionsSpec extends FlatSpec with Matchers{
  val password =  new PasswordOptions()

  "Input" should "be parsed to two ints" in {
    178416 shouldBe password.lower
    676461 shouldBe password.upper
  }

  "IsPasswordOption" should "adhere to bounds" in {
    val pw1 = 12749159

    false shouldBe password.isPasswordOption(pw1)

    val pw2 = 228888

    true shouldBe password.isPasswordOption(pw2)

//    val pw1 = 12749159

//    false shouldBe password.isPasswordOption(pw1)
  }

  "hasDoubleDigits" should "adhere to examples" in {
    val singleDigits = 10
    false shouldBe password.hasDoubleDigit(singleDigits)

    val singleDigits2 = 109
    false shouldBe password.hasDoubleDigit(singleDigits2)

    val singleDigits3 = 123456
    false shouldBe password.hasDoubleDigit(singleDigits3)

    val doubleDigits1 = 11
    true shouldBe password.hasDoubleDigit(doubleDigits1)

    val doubleDigits2 = 211
    true shouldBe password.hasDoubleDigit(doubleDigits2)

    val mutlipleDigits1 = 188888
    false shouldBe password.hasDoubleDigit(mutlipleDigits1)

    val multipleDigits2 = 123444
    false shouldBe password.hasDoubleDigit(multipleDigits2)

    val multipleDigits3 = 111122
    true shouldBe password.hasDoubleDigit(multipleDigits3)

    val multipleDigits4 = 221111
    true shouldBe password.hasDoubleDigit(multipleDigits4)

    val multipleDigits5 = 112233
    true shouldBe password.hasDoubleDigit(multipleDigits5)

    val multipleDigits6 = 123334
    false shouldBe password.hasDoubleDigit(multipleDigits6)
  }

  "hasDecreasingPairs" should "adhere to examples" in {
    val decreasingPair1 = 10
    true shouldBe password.hasDecreasingPairs(decreasingPair1)

    val decreasingPair2 = 223450
    true shouldBe password.hasDecreasingPairs(decreasingPair2)

    val decreasingPair3 = 5034223
    true shouldBe password.hasDecreasingPairs(decreasingPair3)

    val increasingPairs1 = 188888
    false shouldBe password.hasDecreasingPairs(increasingPairs1)

    val increasingPairs2 = 123456
    false shouldBe password.hasDecreasingPairs(increasingPairs2)
  }

  "isPasswordOption" should "adhere to examples" in {
    val correct1 = 222222
    true shouldBe password.isPasswordOption(correct1)

    val correct2 = 223344
    true shouldBe password.isPasswordOption(correct2)

    val incorrect1 = 223450
    false shouldBe password.isPasswordOption(incorrect1)

    val incorrect2 = 123789
    false shouldBe password.isPasswordOption(incorrect2)
  }
}
