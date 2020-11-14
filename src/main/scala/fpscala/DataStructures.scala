package fpscala

import fpscala.List._

object DataStructures {
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
}

