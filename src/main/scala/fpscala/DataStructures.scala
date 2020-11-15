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
}

