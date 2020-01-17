package hw3

import hw3.Main.romanji
import org.scalatest.{FunSuite, Matchers}

class RomanjiTest extends FunSuite with Matchers {
  test("Toilet")     { romanji("トイレ") shouldBe "toire"  }
  test("Television") { romanji("テレビ") shouldBe "terebi" }
  test("Drama")      { romanji("ドラマ") shouldBe "dorama" }
  test("Ice-cream")  { romanji("アイスクリーム") shouldBe "aisukurīmu" }
  test("Knock")      { romanji("ノック") shouldBe "nokku"}
  test("kya")        { romanji("キヤ") shouldBe "kya"}
}