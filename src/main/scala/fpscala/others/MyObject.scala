package fpscala.others

object MyObject {
  def combine[A](as: Traversable[Traversable[A]]): Seq[Seq[A]] =
    as.foldLeft(Seq(Seq.empty[A])) {
      (x, y) => for (a <-x.view; b <-y ) yield a :+ b
    }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def seqO(xs: Seq[String]): Option[Seq[String]] =
    Option(xs).filter(_.nonEmpty)
}
