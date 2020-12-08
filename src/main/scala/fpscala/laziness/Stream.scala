package fpscala.laziness

import scala.collection.immutable.List
import fpscala.laziness.Stream._


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
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
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
    foldRight(true)((a, b) => p(a) && b)

  /**
   * Exercise 5.5
   * Use foldRight to implement takeWhile.
   */
  def takeWhileViafoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) =>
        if (f(h)) cons(h, t)
        else empty
    )

  /**
   * Exercise 5.6
   * Implement headOption using foldRight, which optionally extract the head of a Stream.
   */
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  /**
   * Exercise 5.7
   * Implement map, filter, append, and flatMap using foldRight.
   */
  // Returns the stream resulting from applying the given function f to each element of this stream.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  // Selects all elements of this traversable collection which satisfy a predicate.
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) =>
        if (p(h)) cons(h, t)
        else t
    )

  // The stream resulting from the concatenation of this stream with the argument stream.
  // B>:A is a lower type bound, which means that B is constrained to be supertype of A
  // B<:A is an upper type bound, which means that B is constrained to be subtype of A
  def append[B >: A](rest: => Stream[B]): Stream[B] =
    foldRight(rest)(
      (h, t) => cons(h, t)
    )

  // Applies the given function f to each element of this stream, then concatenates the results.
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  /**
   * Exercise 5.13
   * Use unfold to implement map, take, takeWhile, zipWith, and zipAll.
   */
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t), 1) => Some((h(), (empty, 0)))
      case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // Returns a immutable sequence formed from this immutable sequence and another iterable collection by combining corresponding elements in pairs.
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  // Returns a immutable sequence formed from this immutable sequence and another iterable collection by combining corresponding elements in pairs.
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  /**
   * Exercise 5.14
   * Implement startsWith.
   */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  /**
   * Exercise 5.15
   * Implement tails using unfold.
   * For a given Stream, tails returns the Stream of suffixes of the input sequence, starting with the original Stream.
   */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  /**
   * Here `b` is the unevaluated recursive step that folds the tail of the stream.
   * If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
   */
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  /**
   * Exercise 5.16
   * Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the intermediate results.
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a,p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
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

  /**
   * Exercise 5.8
   * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
   */
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /**
   * Exercise 5.9
   * Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.
   */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /**
   * Exercise 5.10
   * Write a function fibs that generates the infinite stream of Fibonacci numbers:0, 1, 1, 2, 3, 5, 8, and so on.
   */
  def fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  /**
   * Exercise 5.11
   * Write a more general stream-building function called unfold.
   * It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  /**
   * Exercise 5.12
   * Write fibs, from, constant, and ones in terms of unfold.
   */
  def fibsViaUnfold =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))

}
