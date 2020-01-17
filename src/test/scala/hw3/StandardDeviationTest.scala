package hw3

import hw3.Main.standardDeviation
import org.scalatest.{FunSuite, Matchers}

class StandardDeviationTest extends FunSuite with Matchers {
  test("stddev example")     {
    /**
     * x	                 4      9    11   12    17     5     8    12    14
     * (x - avg(x))^2   38.7  1.49  0.60  3.16  45.9  27.3  4.94  3.16  14.3
     *
     * sum:    sum(x) = 139.55
     * avg:    139.55/9 = 15.51
     * stddev: sqrt(15.51) = 3.94
     */
    standardDeviation(List(4, 9, 11, 12, 17, 5, 8, 12, 14)) shouldBe 3.93 +- 0.01
  }
  test("stddev singleton example")     {
    /**
     * x	                  42
     * (x - avg(x))^2        0
     * stddev: sqrt(0) = 0
     */
    standardDeviation(List(42)) shouldBe 0.0
  }

  test("test null vector")     {

    assertThrows[IllegalArgumentException]{
      standardDeviation(null)
    }
  }

  test("test empty vector")     {

    assertThrows[IllegalArgumentException]{
      standardDeviation(List[Double]())
    }
  }



}
