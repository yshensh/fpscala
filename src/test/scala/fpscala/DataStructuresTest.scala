package fpscala

import org.scalatest.{FunSuite, Matchers}
import fpscala.datastructures.List._
import fpscala.datastructures.Tree._
import fpscala.datastructures.{Cons, Nil, List, Leaf, Branch, Tree}


class DataStructuresTest extends FunSuite with Matchers {
  test("exercise 3.1") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x  // match if x = List(1, 2, 4, 5) then x = 1
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // match x = 3
      case Cons(h, t) => h + sum(t) // match but not the first match x = 1 + 2 + 3 + 4 + 5
      case _ => 101 // otherwise x = 101
    }
    x shouldBe (3)
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

  test("exercise 3.4") {
    drop(List(1, 2, 3), 3) shouldBe Nil
    drop(List(1, 2, 3), 5) shouldBe Nil
    drop(List(1, 2, 3), 1) shouldBe List(2, 3)
  }

  test("exercise 3.5") {
    dropWhile(List(1, 3, 5), (x: Int) => (x % 2 != 0)) shouldBe Nil
    dropWhile(List(1, 2, 3), (x: Int) => (x % 2 != 0)) shouldBe List(2, 3)
    dropWhile(List(2, 4, 6), (x: Int) => (x % 2 != 0)) shouldBe List(2, 4, 6)
  }

  test("exercise 3.6") {
    init(List(1, 2, 3)) shouldBe List(1, 2)
    val caught = intercept[RuntimeException] {
      init(Nil)
    }
    assert(caught.getMessage.equals("init of empty list"))
  }

  test("sumR implemented with foldRight") {
    // trace the evaluation of foldRight used in sum
    sumR(List(1, 3, 5)) shouldBe 1 + (3 + foldRight(Cons(5, Nil), 0)((x, y) => x + y))
  }

  test("exercise 3.9") {
    List.length(List(1, 2, 3)) shouldBe 3
    List.length(Nil) shouldBe 0
  }

  test("exercise 3.10 and 3.11") {
    sumL(List(1, 3, 5)) shouldBe 9
    product(List(1, 3, 5)) shouldBe 15
    lengthL(List(1, 2, 3)) shouldBe 3
  }

  test("exercise 3.12") {
    // trace the evaluation of foldLeft used in reverse
    reverse(List(1, 3, 5)) shouldBe Cons(5, Cons(3, Cons(1, List[Int]())))
  }

  test("exercise 3.14") {
    // trace the evaluation of foldRight used in append
    appendViaFoldRight(List(1, 3), List(5, 7, 9)) shouldBe Cons(1, Cons(3, List(5, 7, 9)))
    // trace the evaluation of foldLeft used in append
    appendViaFoldLeft(List(1, 3), List(5, 7, 9)) shouldBe Cons(1, Cons(3, List(5, 7, 9)))
  }

  test("exercise 3.15") {
    // trace the evaluation of foldRight used in concat
    concat(List(List(1, 3), List(5, 7, 9), List(11))) shouldBe appendViaFoldRight(List(1, 3), appendViaFoldRight(List(5, 7, 9), appendViaFoldRight(List(11), Nil)))
  }

  test("exercise 3.16") {
    // trace the evaluation of foldRight used in addOne
    addOne(List(0, 1, 2)) shouldBe (Cons(0 + 1, Cons(1 + 1, Cons(2 + 1, Nil))))
  }

  test("exercise 3.17") {
    doubleToString(List(0.1, 0.2 , 0.3)) shouldBe List("0.1", "0.2", "0.3")
  }

  test("exercise 3.20") {
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  test("exercise 3.22") {
    addPairwise(List(1, 2, 3), List(5, 7, 9)) shouldBe List(6, 9, 12)
  }

  test("exercise 3.23") {
    zipWith(List("A", "B", "C"), List("a", "b", "c"))(_ + _) shouldBe List("Aa", "Bb", "Cc")
  }

  test("exercise 3.24") {
    def l = List("a", "b", "c", "d", "e")
    hasSubsequence(l, List("b", "c")) shouldBe true
    hasSubsequence(l, List("a", "b")) shouldBe true
    hasSubsequence(l, Nil) shouldBe true
  }

  test("exercise 3.25") {
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.size(t) shouldBe 5
  }

  test("exercise 3.26") {
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    maximum(t) shouldBe 3
  }

  test("exercise 3.27") {
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    depth(t) shouldBe 2
  }

  test("exercise 3.28") {
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(5))
    mapForTree(t)(_ % 2 ==0) shouldBe Branch(Branch(Leaf(false),Leaf(true)),Leaf(false))
  }

  test("exercise 3.29") {
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    sizeViaFold(t) shouldBe 5
    maximumViaFold(t) shouldBe 3
    depthViaFold(t) shouldBe 2
    mapViaFold(t)(_ % 2 == 0) shouldBe Branch(Branch(Leaf(false),Leaf(true)),Leaf(false))
  }
}
