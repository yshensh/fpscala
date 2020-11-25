package fpscala

sealed trait Option[+A]
{
  /**
   * Exercise 4.1
   * Implement all of the functions on Option.
   */
  // Apply f if the option is not None
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // returns the result inside the Some case of the Option, or if the Option is None, returns the given default value.
  // B: > A says that the B type parameter must be a supertype of A.
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }


  // Apply f, which may fail, to the Option if not None
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  //  def flatMap[B](f: A => Option[B]): Option[B] = this match {
  //    case None => None
  //    case Some(a) => f(a)
  //  }


  // returns the first Option if it's defined; otherwise, it returns the second Option
  // Don't evaluate ob unless needed.
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  //  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
  //    case None => ob
  //    case _ => this
  //  }


  // Convert Some to None if the value doesn't satisfy f.
  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  //  def filter(f: A => Boolean): Option[A] = this match {
  //    case Some(a) if f(a) => this
  //    case _ => None
  //  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  /**
   * exercise 4.2
   * Implement the variance function in terms of flatMap.
   */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map (x => math.pow(x - m, 2))))
  // equivalent infix notation
//  mean(xs).flatMap(m => mean(xs.map (x => math.pow(x - m, 2))))


  /**
   * exercise 4.3
   * Write a generic function map2 that combines two Option values using a binary function.
   * if we write
   * a map (aa => b map (bb => f(aa, bb)))
   *
   * bb => f(aa, bb) returns Option[C]
   * a map (aa => ...) returns Option[C]
   * the result becomes Option[Option[C]]
   *
   * change it to
   * a flatMap (aa => ...)
   * this unpacks double Option into a simple Option[C]
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))
}
