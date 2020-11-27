package fpscala

import org.scalatest.{FunSuite, Matchers}
import fpscala.Option._

import scala.collection.immutable.{List => ScalaList}
import scala.util.{Success, Try}

class ErrorHandlingTest extends FunSuite with Matchers{
  test("exercise 4.1") {
    case class Employee(name: String, department: String, manager: Option[String])

    def lookupByName(name: String): Option[Employee] = name match {
      case "Joe" => Some(Employee("Joe", "Accounting", Some("Mike")))
      case "Peter" => Some(Employee("Peter", "Engineering", None))
      case _ => None
    }

    // map
    // Joe's dept. if Joe is an employee
    lookupByName("Joe").map(_.department) shouldBe Some("Accounting")
    // None if Joe is not an employee
    lookupByName("John").map(_.department) shouldBe None

    // flatMap
    // Some(manager) if Joe has a manager
    lookupByName("Joe").flatMap(_.manager) shouldBe Some("Mike")
    // None if Joe is not an employee or doesn't have a manager
    lookupByName("Peter").flatMap(_.manager) shouldBe None

    // getOrElse
    // Joe's department if he has one
    lookupByName("Joe").map(_.department).getOrElse("CEO") shouldBe "Accounting"
    // "Default dept." if not
    lookupByName("Mike").map(_.department).getOrElse("Default dept.")

    // orElse
    // Joe's department if he has one
    lookupByName("Joe").map(_.department).orElse(Some("Default dept.")) shouldBe Some("Accounting")
    // "Default dept." if not
    lookupByName("Mike").map(_.department).orElse(Some("Default dept."))
  }

  test("exercise 4.4") {
    sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    sequence(List(Some(1), Some(2), None)) shouldBe None

    sequence_1(ScalaList(Some(1), Some(2), Some(3))) shouldBe Some(ScalaList(1, 2, 3))
    sequence_1(ScalaList(Some(1), Some(2), None)) shouldBe None
  }

  test("exercise 4.5") {
    val list1 = List("1", "2", "3")
    val list2 = List("1", "b", "3")

    def parseInt(a: String):  Option[Int] =
      Try(a.toInt) match {
        case Success(r) => Some(r)
        case _=> None
      }

    traverse(List("1", "2", "3"))(i => parseInt(i)) shouldBe Some(List(1, 2, 3))
    traverse(List("1", "b", "3"))(i => parseInt(i)) shouldBe None

    traverse_1(ScalaList("1", "2", "3"))(i => parseInt(i)) shouldBe Some(ScalaList(1, 2, 3))
    traverse_1(ScalaList("1", "b", "3"))(i => parseInt(i)) shouldBe None

    sequenceViaTraverse(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    sequenceViaTraverse(List(Some(1), Some(2), None)) shouldBe None
  }
}
