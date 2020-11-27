package fpscala

sealed trait Either[+E, +A] {
  /**
   * Exercise 4.6
   * Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
   *
   * The term B >: A expresses that the type parameter B or the abstract type B refer to a supertype of type A.
   *
   * A type parameter T of a generic class can be made covariant by using the annotation +T
   * For some class List[+T], making T covariant implies that for two types A and B where B is a subtype of A, then List[B] is a subtype of List[A].
   * This allows us to make very useful and intuitive subtyping relationships using generics.
   *
   */

  // The given function is applied if this is Right
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  // Binds the given function across Right
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  // Returns this Right or the given argument if this is a Left.
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }

  // Combines two Either using a binary function
  // for-comprehension that expands automatically to a series of flatMap and map calls
  // The compiler desugars the bindings to flatMap calls, with the final binding and yield being converted to a call to map.
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

  def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap( aa => b map (bb => f(aa, bb)))

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

}
