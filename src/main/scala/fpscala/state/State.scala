package fpscala.state

import State._

case class State[S, +A](run: S => (A, S)) {

  /**
   * Exercise 6.10
   * Generalize the functions unit, map, map2, flatMap, and sequence.
   */
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap((a => sb.map(b => f(a,b))))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  )

}

object State {
  /**
   * Exercise 6.10
   * Generalize the functions unit, map, map2, flatMap, and sequence.
   */
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((cur, acc) => cur.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get  // gets the current state and assigns it to s
    _ <- set(f(s)) // sets the new state to f applied to s
  } yield()
}


