package fpscala

import org.scalatest.{FunSuite, Matchers}
import fpscala.Option._
import scala.collection.immutable.{List => ScalaList}

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
}
