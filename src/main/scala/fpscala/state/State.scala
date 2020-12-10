package fpscala.state

case class State[S, +A](run: S => (A, S)) {

  /**
   * Exercise 6.10
   * Generalize the functions unit, map, map2, flatMap, and sequence.
   */
  def map[B](f: A => B): State[S, B] = ???

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ???

  def flatMap[B](f: A => State[S, B]): State[S, B] = ???

}

object State {
  /**
   * Exercise 6.10
   * Generalize the functions unit, map, map2, flatMap, and sequence.
   */
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = ???

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = ???
}


