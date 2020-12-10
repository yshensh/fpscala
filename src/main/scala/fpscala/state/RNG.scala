package fpscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  /**
   * Exercise 6.1
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue(inclusive).
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  /**
   * Exercise 6.2
   * Write a function to generate a Double between 0 and 1, not including 1.
   */
  def double(rng: RNG): (Double, RNG) = ???

  /**
   * Exercise 6.3
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???

  /**
   * Exercise 6.4
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

  /**
   * Exercise 6.5
   * Use map to reimplement double in a more elegant ways.
   */
  def doubleViaMap(rng: RNG): (Double, RNG) = ???

  /**
   * Exercise 6.6
   * Write a function map2 that takes two action, ra and rb, and a function f for combining their results, and returns a new action that combines them.
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  /**
   * Exercise 6.7
   * Implement sequence for combining a List of transitions into a single transition.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  /**
   * Exercise 6.8
   * Implement flatMap, and then use it to implement nonNegativeLessThan.
   */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  def nonNegativeLessThan(rng: RNG): (Int, RNG) = ???

  /**
   * Exercise 6.9
   * Reimplement map and map2 in terms of flatMap.
   */
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = ???

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???


}
