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

  /**
   * Applies a binary operator to all elements of this sequence and a start value, going right to left.
   *
   * f takes two arguments: 1) A and 2) => B
   * The arrow => in front of the argument type B means that the function f takes its second argument
   * by name and may choose not to evaluate it.
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      // If f doesn't evaluate its second argument, the recursion never occurs.
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  /**
   * Exercise 5.4
   * Implement forAll, which checks that all elements in the Stream match a given predicate.
   */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  /**
   * Exercise 5.5
   * Use foldRight to implement takeWhile.
   */
  def takeWhileViafoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) =>
        if (f(h)) cons(h,t)
        else empty
    )

  /**
   * Exercise 5.6
   * Implement headOption using foldRight, which optionally extract the head of a Stream.
   */
  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  /**
   * Exercise 5.7
   * Implement map, filter, append, and flatMap using foldRight.
   */
  // Returns the stream resulting from applying the given function f to each element of this stream.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  // Selects all elements of this traversable collection which satisfy a predicate.
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h,t) =>
        if(p(h)) cons(h, t)
        else t
    )

  // The stream resulting from the concatenation of this stream with the argument stream.
  // B>:A is a lower type bound, which means that B is constrained to be supertype of A
  // B<:A is an upper type bound, which means that B is constrained to be subtype of A
  def append[B>:A](rest: => Stream[B]): Stream[B] =
    foldRight(rest)(
      (h,t) => cons(h, t)
    )

  // Applies the given function f to each element of this stream, then concatenates the results.
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

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
