package fpscala

import fpscala.laziness.{Stream}

import scala.collection.immutable.List
import org.scalatest.{FunSuite, Matchers}


class Laziness extends FunSuite with Matchers {
  test("exercise 5.1") {
    Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
  }

  test("exercise 5.2") {
    Stream("a", "b", "c").take(1).toList shouldBe List("a")
    Stream("a", "b", "c").take(2).toList shouldBe List("a", "b")

    Stream("a", "b", "c").drop(1).toList shouldBe List("b", "c")
    Stream("a", "b", "c").drop(2).toList shouldBe List("c")
  }

  test("exercise 5.3") {
    Stream(2, 1, 3).takeWhile(x => x % 2 == 0).toList shouldBe List(2)
    Stream(1, 2, 3).takeWhile(x => x % 2 == 0).toList shouldBe List()
    Stream(1, 2, 3).takeWhile(x => x % 2 != 0).toList shouldBe List(1)
    Stream(1, 3, 2).takeWhile(x => x % 2 != 0).toList shouldBe List(1, 3)
  }
}
