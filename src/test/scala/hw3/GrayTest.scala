package hw3

import hw3.Main.gray
import org.scalatest.{FunSuite, Matchers}

class GrayTest extends FunSuite with Matchers {

  test("Gray example 1 bit") {
    gray(1) shouldBe List("0", "1")
  }

  test("Gray example 2 bits") {
    gray(2) shouldBe List("00", "01", "11", "10")
  }

  test("Gray example 3 bits") {
    gray(3) shouldBe List("000", "001", "011", "010", "110", "111", "101", "100")
  }

  test("negative bits") {
    assertThrows[IllegalArgumentException] {
      gray(-5)
    }
  }

  test("zero bits") {
    assertThrows[IllegalArgumentException] {
      gray(0)
    }
  }
}