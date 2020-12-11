package fpscala

import fpscala.state.RNG.SimpleRNG
import fpscala.state.RNG._
import org.scalatest.{FunSuite, Matchers}


class StateTest extends FunSuite with Matchers {
  test("exercise 6.1") {
    val (res1, rng1) = nonNegativeInt(SimpleRNG(15))
    val (res2, rng2) = nonNegativeInt(rng1)
    (res1 >= 0 && res1 <=Int.MaxValue) shouldBe true
    (res2 >= 0 && res2 <=Int.MaxValue) shouldBe true
    (res1 != res2) shouldBe true
  }

  test("exercise 6.2") {
    val (res1, rng1) = double(SimpleRNG(15))
    val (res2, rng2) = double(rng1)
    (res1 >= 0 && res1 < 1) shouldBe true
    (res2 >= 0 && res2 < 1) shouldBe true
    (res1 != res2) shouldBe true
  }


}
