package fpscala

import org.scalatest.{FunSuite, Matchers}
import fpscala.List._
import DataStructures._


class DataStructuresTest extends FunSuite with Matchers {
  test("exercise 3.1") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x  // match if x = List(1, 2, 4, 5) then x = 1
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // match x = 3
      case Cons(h, t) => h + sum(t) // match but not the first match x = 1 + 2 + 3 + 4 + 5
      case _ => 101 // otherwise x = 101
    }
    x should be (3)
  }

  test("exercise 3.2") {
    tail(List(1, 2, 3)) shouldBe List(2, 3)

    val caught = intercept[RuntimeException] {
      tail(Nil)
    }
    assert(caught.getMessage.equals("tail of empty list"))
  }

  test("exercise 3.3") {
    setHead(List(1, 2, 3), 0) shouldBe List(0, 2, 3)

    val caught = intercept[RuntimeException] {
      setHead(Nil, 0)
    }
    assert(caught.getMessage.equals("setHead on empty list"))
  }

}
