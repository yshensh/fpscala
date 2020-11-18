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

  def sumR(ns: List[Int]) =
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


}

