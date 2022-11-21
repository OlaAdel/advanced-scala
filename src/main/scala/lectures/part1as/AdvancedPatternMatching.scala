package lectures.part1as

object AdvancedPatternMatching extends App {

  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _ =>
  }
  /*
    - constants
    - wildcards
    - case classes
    - tuples
    - some special magic like above
   */

  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None
      else Some((person.name, person.age))

    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 25) // age 20 will throw an error
  val greeting = bob match {
    case Person(name, age) => s"Hi, my name is $name and I am $age yo."
  }
  println(greeting)

  val legalStatus = bob.age match {
    case Person(status) => s"my legal status is $status"
  }
  println(legalStatus)

  /*
    Exercises
   */

  val n: Int = 45
  val mathProperty = n match {
    case x if x < 10 => "single digit"
    case x if x % 2 == 0 => "en even number"
    case _ => "no property"
  }

  object SingleDigit {
    def unapply(n: Int): Boolean =
      (n > -10 && n < 10)
  }

  object EvenNumber {
    def unapply(n: Int): Boolean =
      (n % 2 == 0)
  }

  val smartMathProperty = n match {
    case SingleDigit() => "single digit"
    case EvenNumber() => "en even number"
    case _ => "no property"
  }

  println(smartMathProperty)

  // infix patterns, it only works when you have 2 things
  case class Or[A, B](a: A, b: B) //Either

  val either = Or(2, "two")
  val humanDescription = either match {
    case Or(number, string) => s"$number is written as $string"
    case number Or string => s"$number is written as $string"
  }
  println(humanDescription)


  // decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  abstract class MyList[+A] {
    def head: A = ???

    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]

  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1, 2"
    case MyList(1, 2, 3) => "starting with 1, 2"
    case _ => "something else"
  }
  println(decomposed)


  // custom return types for unapply
  // isEmpty: Boolean, get: something

  abstract class Wrapper[T] {
    def isEmpty: Boolean

    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false

      override def get: String = person.name
    }
  }

  println(bob match {
    case PersonWrapper(name) => s"This person name is $name"
    case _ => "An alien"
  })

}
