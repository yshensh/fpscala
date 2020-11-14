package fpscala

import org.scalatest.FunSuite
import org.scalatest.Matchers
import fpscala.Gettingstarted._


class GettingstartedTest extends FunSuite with Matchers {
  test("exercise 2.1") {
    fib(5) should be (5)
    assert(fib(5) === 5)
  }

  test("exercise 2.2") {
    isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x < y) shouldBe true

    isSorted(Array(5, 4, 3, 2, 1), (x: Int, y: Int) => x > y) shouldBe true

    isSorted(Array("Apple", "Orange", "Kiwi"), (x: String, y: String) => x.length < y.length) shouldBe false
  }
}
