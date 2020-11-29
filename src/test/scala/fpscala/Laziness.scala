package fpscala

import fpscala.laziness.Stream._

import scala.collection.immutable.List
import org.scalatest.{FunSuite, Matchers}


class Laziness extends FunSuite with Matchers {
  test("exercise 5.1") {
    laziness.Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
  }

}
