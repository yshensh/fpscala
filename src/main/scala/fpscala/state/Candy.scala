package fpscala.state

import fpscala.state.State._

/**
 * Exercise 6.11
 * Implement a finite state automation that models a simple candy dispenser.
 * The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
 * It can be in one of two states: locked or unlocked.
 * It also tracks how many candies are left and how many coins it contains
 */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      // a machine that's out of candy ignores all inputs.
      case (_,Machine(_, 0, _)) => s
      // turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      // inserting a coin into a locked machine will cause it to unlock if there's any candy left
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      // turning the knob on an unlocked machine will cause it to dispense candy and become locked
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}
