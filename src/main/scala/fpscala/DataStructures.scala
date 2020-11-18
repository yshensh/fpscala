package fpscala

import fpscala.List._

object DataStructures {
  // 3.3 Data sharing in functional data structures
  /***
   * exercise 3.2
   * Implement the function tail for removing the first element of a List
   */
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, tail) => tail
    }


  /***
   * exercise 3.3
   * Implement the function setHead for replacing the first element of a List with a different value
   */
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, tail) => Cons(h, tail)
    }


  /**
   * exercise 3.4
   * Generalize tail to the function drop, which removes the first n elements from a list.
   */
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n-1)
    }


  /**
   * exercise 3.5
   * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
   */
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l
    }


  /**
   * exercise 3.6
   * Implement a function, init, that returns a List consisting of all but the last element of a List
   */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }


  /**
   * foldRight and sum implemented in foldRight
   */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sumR(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  /**
   * exercise 3.9
   * Compute the length of a list using foldRight.
   */
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_,acc) => acc + 1)


  /**
   * exercise 3.10
   */
  @scala.annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }


  /**
   * exercise 3.11
   * Write sum, product, and a function to compute the length of a list using foldLeft
   */
  def sumL(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def productL(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthL[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, h) => acc + 1)


  /**
   * exercise 3.12
   * Write a function that returns the reverse of a list
   */
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, h) => Cons(h, acc))


  /**
   * exercise 3.13
   * Write foldLeft in terms of foldRight and foldRight in terms of foldLeft
   *
   * - foldLeft in terms of foldRight
   * Our foldLeft(List(1, 2, 3), z)(*) is ((z * 1) * 2) * 3
   * Types here: List(1, 2, 3) is type List[A]
   * z is of type B
   * * is of type (B, A) -> B
   *
   * Result is of type B
   *
   * We want to express that in terms of foldRight
   * As above:
   * f0 = identity. f0(t) = t.
   * f1 = g(3, f0). So f1(t) = f0(t * 3) = t * 3
   * f2 = g(2, f1). So f2(t) = f1(t * 2) = (t * 2) * 3
   * f3 = g(1, f2). So f3(t) = f2(t * 1) = ((t * 1) * 2) * 3
   *
   * And finally we apply f3 on z and get the expression we want.
   * f3 = g(1, g(2, g(3, f0)))
   * which means f3 = foldRight(as, f0)(g)
   *
   */
  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
  //    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
  {
    val f0 = (b: B) => b

    /**
     * first arg to g is of type A
     * second arg to g is of the type of these s's, which is B => B
     * So type of g is (A, (B=>B)) => (B=>B)
     */
    def g(a: A, s: B=>B): B=> B =
      t => s(f(t, a))

    foldRight(as, f0)(g)(z)
  }


  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  /**
   * exercise 3.14
   * Implement append in terms of either foldLeft or foldRight.
   */
  def appendViaFoldRight[A](as: List[A], r: List[A]): List[A] =
    foldRight(as, r)((h,acc) => Cons(h, acc))

  def appendViaFoldLeft[A](as: List[A], r: List[A]): List[A] =
    foldLeft(reverse(as), r)((acc, h) => Cons(h, acc)

  /**
   * exercise 3.15
   * Write a function that concatenates a list of lists into a single list.
   * Its runtime should be linear in the total length of all lists.
   */

}

