package fpscala

import org.scalatest.FunSuite

class VarianceTest extends FunSuite {
  test("covariance") {
    /**
     * A type parameter T of a generic class can be made covariant by using the annotation +T.
     * For some class List[+T], making T covariant implies that for two types A and B where B is a subtype of A, then List[B] is a subtype of List[A].
     */
    abstract class Animal {
      def name: String
    }
    case class Cat(name: String) extends Animal
    case class Dog(name: String) extends Animal

    def printAnimalNames(animals: List[Animal]): Unit =
      animals.foreach {
        animal => println(animal.name)
      }

    val cats: List[Cat] = List(Cat("Whiskers"), Cat("Tom"))
    val dogs: List[Dog] = List(Dog("Fido"), Dog("Rex"))

    // List[A] is covariant, the following two method calls can compile
    printAnimalNames(cats)
    printAnimalNames(dogs)
  }

  test("contravariance") {
    /**
     * for class Printer[-A], making A contravariant implies that:
     * for two types A and B where A is a subtype of B
     * Printer[B] is a subtype of Printer[A].
     */
    abstract class Animal {
      def name: String
    }
    case class Cat(name: String) extends Animal
    case class Dog(name: String) extends Animal

    abstract class Printer[-A] {
      def print(value: A): Unit
    }

    class AnimalPrinter extends Printer[Animal] {
      def print(animal: Animal): Unit =
        println("The animal's name is: " + animal.name)
    }

    class CatPrinter extends Printer[Cat] {
      def print(cat: Cat): Unit =
        println("The cat's name is: " + cat.name)
    }

    def printMyCat(printer: Printer[Cat], cat: Cat): Unit =
      printer.print(cat)

    val catPrinter: Printer[Cat] = new CatPrinter
    val animalPrinter: Printer[Animal] = new AnimalPrinter

    // If Printer[A] is not contravariant, Printer[Cat] does not know how to print any Animal to the console.
    // Since Printer[A] is contravariant, we are able to use a Printer[Animal] in place of Printer[Cat]
    printMyCat(catPrinter, Cat("Boots"))
    printMyCat(animalPrinter, Cat("Boots"))
  }
}
