package fpscala

import scala.annotation.tailrec


object Gettingstarted {
  /***
   * exercise 2.1
   */
  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }


  /***
   * exercise 2.2
   */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    @tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }
    go(0)
  }

}
