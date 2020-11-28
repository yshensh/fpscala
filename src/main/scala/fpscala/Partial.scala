package fpscala

trait Partial[+A,+B] {
  // The given function is applied if this is Partial
  def map[C](f: B => C): Partial[A, C] =
    this match {
      case Success(b) => Success(f(b))
      case Errors(e) => Errors(e)
    }

  // Binds the given function across Partial
  def flatMap[AA >: A, C](f: B => Partial[AA, C]): Partial[AA, C] =
    this match {
      case Success(b) => f(b)
      case Errors(e) => Errors(e)
    }

  def map2[AA >: A, C, D](b: Partial[AA, C])(f: (B, C) => D): Partial[AA, D] =
    (this, b) match {
      case (Errors(e1), Errors(e2)) => Errors(e1 ++ e2)
      case _ => this.flatMap(aa=>b.map(bb=>f(aa,bb)))
    }
}
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]
