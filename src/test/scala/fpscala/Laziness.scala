package fpscala

import fpscala.laziness.{Cons, Stream}

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

  test("exercise 5.4") {
    Stream(1, 3, 5, 7, 9).forAll(x => x % 2 == 0) shouldBe false
    Stream(1, 3, 5, 7, 9).forAll(x => x % 2 != 0) shouldBe true
    Stream(1, 2, 3).forAll(x => x % 2 == 0) shouldBe false
  }

  test("exercise 5.5") {
    Stream(2, 1, 3).takeWhileViafoldRight(x => x % 2 == 0).toList shouldBe List(2)
    Stream(1, 2, 3).takeWhileViafoldRight(x => x % 2 == 0).toList shouldBe List()
    Stream(1, 2, 3).takeWhileViafoldRight(x => x % 2 != 0).toList shouldBe List(1)
    Stream(1, 3, 2).takeWhileViafoldRight(x => x % 2 != 0).toList shouldBe List(1, 3)
  }

  test("exercise 5.6") {
    Stream(2, 1, 3).headOption shouldBe Some(2)
    Stream().headOption shouldBe None
  }

  test("exercise 5.7") {
    /**
     * "first-class loop"
     * the computation alternates between generating a single element of the output of map, and testing with filter to see if that element is divisible by 2 (adding it to the output list if it is).
     * Here we don't fully instantiate teh intermediate stram that results from the map.
     */
    Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList shouldBe List(12, 14)
  }
}
