package fpscala

import org.scalatest.{FunSuite, Matchers}
import fpscala.Option._

import scala.collection.immutable.{List => ScalaList}
import scala.util.{Success => ScalaSuccess, Try}

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
        case ScalaSuccess(r) => Some(r)
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

  test("exercise 4.7") {
    Either.sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
    Either.sequence(List(Right(1), Right(2), Left("Error"))) shouldBe Left("Error")

    Either.sequence_1(ScalaList(Right(1), Right(2), Right(3))) shouldBe Right(ScalaList(1, 2, 3))
    Either.sequence_1(ScalaList(Right(1), Right(2), Left("Error"))) shouldBe Left("Error")

    def parseInt(a: String): Either[String, Int] =
      Try(a.toInt) match {
        case ScalaSuccess(r) => Right(r)
        case _=> Left("Error")
      }

    Either.traverse(List("1", "2", "3"))(i => parseInt(i)) shouldBe Right(List(1, 2, 3))
    Either.traverse(List("1", "b", "3"))(i => parseInt(i)) shouldBe Left("Error")

    Either.traverse_1(ScalaList("1", "2", "3"))(i => parseInt(i)) shouldBe Right(ScalaList(1, 2, 3))
    Either.traverse_1(ScalaList("1", "b", "3"))(i => parseInt(i)) shouldBe Left("Error")

    Either.sequenceViaTraverse(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
    Either.sequenceViaTraverse(List(Right(1), Right(2), Left("Error"))) shouldBe Left("Error")
  }

  test("exercise 4.8 with Either") {
    case class Person(name: Name, age: Age)
    sealed class Name(val value: String)
    sealed class Age(val value: Int)

    def mkName(name: String): Either[String, Name] =
      if (name == "" || name == null) Left("Name is empty")
      else Right(new Name(name))

    def mkAge(age: Int): Either[String, Age] =
      if (age < 0) Left("Age is out of range.")
      else Right(new Age(age))

    def mkPerson(name: String, age: Int): Either[String, Person] =
      mkName(name).map2(mkAge(age))(Person(_,_))

    // map2 is only able to report one error, even if both the name and the age are invalid.
    mkPerson("", -1) shouldBe Left("Name is empty")
    mkPerson(null, 1) shouldBe Left("Name is empty")
    mkPerson("Joe", -1) shouldBe Left("Age is out of range.")
    mkPerson("Joe", 1) equals Right(Person(new Name("Joe"), new Age(1)))
  }

  test("exercise 4.8 with Partial") {
    case class Person(name: Name, age: Age)
    sealed class Name(val value: String)
    sealed class Age(val value: Int)

    def mkName(name: String): Partial[String, Name] =
      if (name == "" || name == null) Errors(Seq("Name is empty"))
      else Success(new Name(name))

    def mkAge(age: Int): Partial[String, Age] =
      if (age < 0) Errors(Seq("Age is out of range."))
      else Success(new Age(age))

    def mkPerson(name: String, age: Int): Partial[String, Person] =
      mkName(name).map2(mkAge(age))(Person(_,_))

    // map2 is able to report both errors, even if both the name and the age are invalid.
    mkPerson("", -1) shouldBe Errors(Seq("Name is empty", "Age is out of range."))
    mkPerson(null, 1) shouldBe Errors(Seq("Name is empty"))
    mkPerson("Joe", -1) shouldBe Errors(Seq("Age is out of range."))
    mkPerson("Joe", 1) equals Success(Person(new Name("Joe"), new Age(1)))
  }
}
