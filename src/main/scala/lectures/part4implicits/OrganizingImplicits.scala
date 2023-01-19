package lectures.part4implicits

object OrganizingImplicits extends App {


  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  // implicit val normalOrdering: Ordering[Int] = Ordering.fromLessThan(_ < _)

  println(List(1, 4, 5, 3, 2).sorted)

  // scala.Predef automatically imported package


  /*
    implicits(used as implicit parameters)
      - val/var
      - object
      - accessor methods = defs with no parentheses
   */

  // Exercise
  case class Person(name: String, age: Int)

  object Person {
    implicit val alphabeticalOrdering: Ordering[Person] = Ordering
      .fromLessThan(_.name < _.name)
  }

  // this will take the precedence over the one defined in the companion object
  /*implicit val ageOrdering: Ordering[Person] = Ordering
    .fromLessThan(_.age < _.age)
*/

  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 66)
  )

  println(persons.sorted)

  /*
    Implicits scope
      - normal scope = LOCAL SCOPE
      - imported scope
      - companions of all types involved in the method signature
          - List
          - Ordering
          - all the types involved = A or any subtype
   */
  //override def sorted[B >: A](implicit ord: Ordering[B]): C


  object AlphabeticNameOrdering {
    implicit val alphabeticalOrdering: Ordering[Person] = Ordering
      .fromLessThan(_.name < _.name)

  }

  object AgeOrdering {
    implicit val ageOrdering: Ordering[Person] = Ordering
      .fromLessThan(_.age < _.age)

  }

  import AlphabeticNameOrdering._

  println(persons.sorted)

  /*
    Exercise

    - totalPrice = most used (50%)
    - by unit count = 25%
    - by unit price = 25%


   */

  case class Purchase(nUnits: Int, unitPrice: Double) {
    val totalPrice: Double = nUnits.doubleValue * unitPrice
  }

  object Purchase {
    implicit val totalPriceOrdering: Ordering[Purchase] =
      Ordering.fromLessThan(_.totalPrice < _.totalPrice)
  }


  object UnitCountOrdering {
    implicit val unitCountOrdering: Ordering[Purchase] =
      Ordering.fromLessThan(_.nUnits < _.nUnits)
  }

  object UnitPriceOrdering {
    implicit val unitPriceOrdering: Ordering[Purchase] =
      Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }

  val purchases = List(
    Purchase(nUnits = 1, unitPrice = 500.5),
    Purchase(nUnits = 4, unitPrice = 250.3),
    Purchase(nUnits = 10, unitPrice = 4.5)
  )

  import UnitPriceOrdering._
  println(purchases.sorted)


}
