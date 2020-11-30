package fpscala.laziness

import scala.collection.immutable.List
import fpscala.laziness.Stream.{cons, empty}


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
  def take(n: Int): Stream[A]=
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

  def drop(n: Int): Stream[A]=
    this match {
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => this
    }

  /**
   * Exercise 5.3
   * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
   */
  def takeWhile(f: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
      case _ => empty
    }
}
case object Empty extends Stream[Nothing]
// A nonempty stream consists of a head and a tail, which are both non-strict.
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // A smart constructor for creating a nonempty stream.
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // cache the head and tail as lazy values to avoid repeated evaluation.
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // A smart constructor for creating an empty stream of a particular type.
  def empty[A]: Stream[A] = Empty

  // A convenient variable-argument method for constructing a Stream from multiple elements.
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
