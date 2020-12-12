package fpscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
   * Exercise 6.1
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue(inclusive).
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    // increment the negative numbers by 1 and make them positive
    // MaxValue: Int(2147483647), MinValue: Int(-2147483648)
    (if (i < 0) -(i + 1) else i, r)
  }

  /**
   * Exercise 6.2
   * Write a function to generate a Double between 0 and 1, not including 1.
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1 ), r)
  }

  /**
   * Exercise 6.3
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
   * Exercise 6.4
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0)
        (List(), rng)
      else {
        val (x, r1) = rng.nextInt
        val (xs, r2) = ints(count - 1)(r1)
        (x :: xs, r2)
      }
    go(count, rng, List())
  }

  /**
   * Exercise 6.5
   * Use map to reimplement double in a more elegant ways.
   */
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleViaMap(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1 ))
  }

  /**
   * Exercise 6.6
   * Write a function map2 that takes two action, ra and rb, and a function f for combining their results, and returns a new action that combines them.
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  /**
   * Exercise 6.7
   * Implement sequence for combining a List of transitions into a single transition.
   */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((cur, acc) => map2(cur, acc)(_ :: _))

  /**
   * Exercise 6.8
   * Implement flatMap, and then use it to implement nonNegativeLessThan.
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  /**
   * Exercise 6.9
   * Reimplement map and map2 in terms of flatMap.
   */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}
