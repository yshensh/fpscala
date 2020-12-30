package fpscala

import org.scalatest.FunSuite
import org.scalatest.Assertions._

class BoundTest extends FunSuite {
  test("upper type bounds") {
    /**
     * An upper type bound `T <: A` declares that type variable T refers to a subtype of type A
     */
    abstract class Animal {
      def name: String
    }

    abstract class Pet extends Animal {}

    class Cat extends Pet {
      override def name: String = "Cat"
    }

    class Dog extends Pet {
      override def name: String = "Dog"
    }

    class Lion extends Animal {
      override def name: String = "Lion"
    }

    class PetContainer[P <: Pet](p: P) {
      def pet: P = p
    }

    val dogContainer = new PetContainer[Dog](new Dog)
    val catContainer = new PetContainer[Cat](new Cat)
    // this would not compile
    // val lionContainer = new PetContainer[Lion](new Lion)
  }

  test("lower type bounds") {
    /**
     * The term B >: A expresses that the type parameter B or the abstract type B refer to a supertype of type A.
     */

    // The class Node and its subtypes are covariant because we have +B
    trait Node[+B] {
      def prepend[U >: B](elem: U): Node[U]
    }

    // U >: A mean U (return value type) is the supertype of B (input value type)
    case class ListNode[+B](h: B, t: Node[B]) extends Node[B] {
      def prepend[U >: B](elem: U): ListNode[U] = ListNode(elem, this)
      def head: B = h
      def tail: Node[B] = t
    }

    case class Nil[+B]() extends Node[B] {
      def prepend[U >: B](elem: U): ListNode[U] = ListNode(elem, this)
    }

    trait Bird
    case class AfricanSwallow() extends Bird
    case class EuropeanSwallow() extends Bird

    // The Node[Bird] can be assigned the africanSwallowList but then accept EuropeanSwallows.
    val africanSwallowList = ListNode[AfricanSwallow](AfricanSwallow(), Nil())
    val birdList: Node[Bird] = africanSwallowList
    birdList.prepend(EuropeanSwallow())
  }
}
