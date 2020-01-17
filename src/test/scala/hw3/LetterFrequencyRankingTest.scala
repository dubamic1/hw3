package hw3

import hw3.Main.letterFrequencyRanking
import org.scalatest.{FunSuite, Matchers}

class LetterFrequencyRankingTest extends FunSuite with Matchers {
  test("Simple") {
    letterFrequencyRanking("hello") shouldBe "leho"
  }
  test("Capital letters") {
    letterFrequencyRanking("AaaAaaAaa") shouldBe "a"
  }
  test("Punctuation") {
    letterFrequencyRanking("Sic!") shouldBe "cis"
  }

  test("Random") {
    letterFrequencyRanking("DB  asd (*^#@$^ 2394623 46234               22    2 c! AA") shouldBe "adbcs"
  }

  test("null") {
    assertThrows[IllegalArgumentException] {
      letterFrequencyRanking(null)
    }
  }
  test("empty String") {
    letterFrequencyRanking("") shouldBe ""
  }


}
