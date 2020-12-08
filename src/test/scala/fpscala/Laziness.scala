package fpscala

import fpscala.laziness.Stream
import fpscala.laziness.Stream._

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

  test("exercise 5.8") {
    constant(1).take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }

  test("exercise 5.9") {
    from(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  test("exercise 5.10") {
    fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  test("exercise 5.12") {
    fibsViaUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
    fromViaUnfold(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
    constantViaUnfold(5).take(3).toList shouldBe List(5, 5, 5)
    onesViaUnfold.take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }

  test("exercise 5.13") {
    Stream("a", "b", "c").zip(onesViaUnfold).toList shouldBe List(("a",1), ("b",1), ("c",1))
    Stream("a", "b", "c").zipAll(onesViaUnfold).take(5).toList shouldBe List((Some("a"),Some(1)), (Some("b"),Some(1)), (Some("c"),Some(1)), (None, Some(1)), (None, Some(1)))
  }

  test("exercise 5.14") {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true
    Stream(0, 1, 2, 3).startsWith(Stream(5, 6)) shouldBe false
    Stream(0, 1, 2, 3).startsWith(Stream(1, 2)) shouldBe false
  }

  test("exercise 5.15") {
    // Iterates over the tails of this imutable sequence
    Stream(1, 2, 3).tails.toList.map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
  }
  test("exercise 5.16") {

    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
  }
}
