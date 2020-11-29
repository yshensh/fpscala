package fpscala.laziness

import scala.collection.immutable.List


sealed trait Stream[+A] {
  /**
   * Exercise 5.1
   * Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the REPL.
   */
  def toList: List[A] =
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List()
    }

  /**
   * Exercise 5.2
   * Write a function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.
   */
  def take(n: Int): Stream[A]= ???

  def drop(n: Int): Stream[A]= ???

  /**
   * Exercise 5.3
   * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
   */
  def takeWhile(p: A => Boolean): Stream[A] = ???



}
case object Empty extends Stream[Nothing]
// A nonempty stream consists of a head and a tail, which are both non-strict.
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
