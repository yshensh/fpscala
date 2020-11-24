package fpscala

import org.scalatest.{FunSuite, Matchers}

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
    print(lookupByName("Mike").map(_.department).getOrElse("Default dept."))

    // getOrElse
    // Joe's department if he has one
    lookupByName("Joe").map(_.department).orElse(Some("Default dept.")) shouldBe Some("Accounting")
    // "Default dept." if not
    print(lookupByName("Mike").map(_.department).orElse(Some("Default dept.")))


  }
}
