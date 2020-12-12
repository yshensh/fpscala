package fpscala

import fpscala.state.{Coin, Machine, Turn}
import fpscala.state.Candy._
import fpscala.state.RNG.SimpleRNG
import fpscala.state.RNG._
import org.scalatest.{FunSuite, Matchers}


class StateTest extends FunSuite with Matchers {
  test("exercise 6.1") {
    val (res1, rng1) = nonNegativeInt(SimpleRNG(15))
    val (res2, rng2) = nonNegativeInt(rng1)
    (res1 >= 0 && res1 <=Int.MaxValue) shouldBe true
    (res2 >= 0 && res2 <=Int.MaxValue) shouldBe true
    (res1 != res2) shouldBe true
  }

  test("exercise 6.2") {
    val (res1, rng1) = double(SimpleRNG(15))
    val (res2, rng2) = double(rng1)
    (res1 >= 0 && res1 < 1) shouldBe true
    (res2 >= 0 && res2 < 1) shouldBe true
    (res1 != res2) shouldBe true
  }

  test("exercise 6.8") {
    val (res1, rng1) = nonNegativeLessThan(5)(SimpleRNG(15))
    val (res2, rng2) = nonNegativeLessThan(5)(rng1)
    (res1 >= 0 && res1 < 5) shouldBe true
    (res2 >= 0 && res2 < 5) shouldBe true
    (res1 != res2) shouldBe true
  }

  test("exercise 6.11") {
    val inputCoin = List(Coin)
    val inputTurn = List(Turn)

    // inserting a coin into a locked machine will cause it to unlock if there's any candy left
    val machine1 = Machine(true, 1, 0)
    val machine1Res = simulateMachine(inputCoin).run(machine1)._2
    machine1Res.locked shouldBe false
    machine1Res.candies shouldBe 1
    machine1Res.coins shouldBe 1

    // a machine that's out of candy ignores all inputs
    val machine2 = Machine(true, 0, 1)
    val machine2CoinRes = simulateMachine(inputCoin).run(machine2)._2
    machine2CoinRes.locked shouldBe true
    machine2CoinRes.candies shouldBe 0
    machine2CoinRes.coins shouldBe 1
    val machine2TurnRes = simulateMachine(inputCoin).run(machine2)._2
    machine2TurnRes.locked shouldBe true
    machine2TurnRes.candies shouldBe 0
    machine2TurnRes.coins shouldBe 1

    val lockedMachine = Machine(true, 20, 5)
    val unlockedMachine = Machine(false, 30, 10)
    // turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing
    val lockedMachineRes1 = simulateMachine(inputTurn).run(lockedMachine)._2
    lockedMachineRes1.locked shouldBe true
    lockedMachineRes1.candies shouldBe 20
    lockedMachineRes1.coins shouldBe 5
    val unlockedMachineRes1 = simulateMachine(inputCoin).run(unlockedMachine)._2
    unlockedMachineRes1.locked shouldBe false
    unlockedMachineRes1.candies shouldBe 30
    unlockedMachineRes1.coins shouldBe 10

    // inserting a coin into a locked machine will cause it to unlock if there's any candy left
    val lockedMachineRes2 = simulateMachine(inputCoin).run(lockedMachine)._2
    lockedMachineRes2.locked shouldBe false
    lockedMachineRes2.candies shouldBe 20
    lockedMachineRes2.coins shouldBe 6

    // turning the knob on an unlocked machine will cause it to dispense candy and become locked
    val unlockedMachineRes2 = simulateMachine(inputTurn).run(unlockedMachine)._2
    unlockedMachineRes2.locked shouldBe true
    unlockedMachineRes2.candies shouldBe 29
    unlockedMachineRes2.coins shouldBe 10
  }
}
