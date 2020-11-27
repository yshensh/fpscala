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
    // None if John is not an employee
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

  test("exercise 4.6") {
    case class Employee(name: String, department: String, manager: Either[String, Employee])

    def lookupByName(name: String): Either[String, Employee] = name match {
      case "Joe" => Right(Employee("Joe", "Accounting", Right(Employee("Mike", "CEO", Left("No manager")))))
      case "Mary" => Right(Employee("Mary", "Accounting", Left("No manager")))
      case "Peter" => Right(Employee("Peter", "Engineering", Left("No manager")))
      case _ => Left("Employee not found")
    }

    // map
    // Joe's dept. if Joe is an employee
    lookupByName("Joe").map(_.department) shouldBe Right("Accounting")
    // None if John is not an employee
    lookupByName("John").map(_.department) shouldBe Left("Employee not found")

    // flatMap
    // Some(manager) if Joe has a manager
    lookupByName("Joe").flatMap(_.manager) shouldBe Right(Employee("Mike", "CEO", Left("No manager")))
    // None if Joe is not an employee or doesn't have a manager
    lookupByName("Peter").flatMap(_.manager) shouldBe Left("No manager")

    // orElse
    // Joe's department if he has one
    lookupByName("Joe").map(_.department).orElse(Left("Default dept.")) shouldBe Right("Accounting")
    // "Default dept." if not
    lookupByName("Mike").map(_.department).orElse(Left("Default dept.")) shouldBe Left("Default dept.")

    // map2
    def employeesShareDepartment(employeeA: Employee, employeeB: Employee) =
      employeeA.department == employeeB.department

    lookupByName("Joe").map2(lookupByName("Mary"))(
      employeesShareDepartment) shouldBe Right(true)
    lookupByName("Joe").map2(lookupByName("Mike"))(
      employeesShareDepartment) shouldBe Left("Employee not found")

    lookupByName("Joe").map2_1(lookupByName("Mary"))(
      employeesShareDepartment) shouldBe Right(true)
    lookupByName("Joe").map2_1(lookupByName("Mike"))(
      employeesShareDepartment) shouldBe Left("Employee not found")
  }
}