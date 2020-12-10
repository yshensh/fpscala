package fpscala.state

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
  def update = (i: Input) => (s: Machine) = ???

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???


}
