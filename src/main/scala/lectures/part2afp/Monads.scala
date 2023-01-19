package lectures.part2afp

object Monads extends App {



  // left-identity

  val adder: Int => List[Int] = x => List(x + 1)
  val multiplier: Int => List[Int] = x => List(x * 2)

  println(List(1).flatMap(adder))
  println(adder(1))

  // right-identity
  println(List(1, 2, 3).flatMap(x => List(x))) // = List(1, 2, 3)

  //associativity
  println(List(1, 2, 3).flatMap(adder).flatMap(multiplier))
  println(List(1, 2, 3).flatMap(adder(_).flatMap(multiplier)))


  // left-identity

  println("Option")

  val adderOption: Int => Option[Int] = x => Some(x + 1)
  val multiplierOption: Int => Option[Int] = x => Some(x * 2)

  println(Some(2).flatMap(adderOption))
  println(adderOption(2))

  // right-identity
  println(Some(2).flatMap(x => Option(x))) //Some(2)

  //associativity
  println(Some(2).flatMap(adderOption).flatMap(multiplierOption))
  println(Some(2).flatMap(adderOption(_).flatMap(multiplierOption)))


  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }


  object Attempt {
    // we call it by name because the a might throw an exception
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Failure(e)
      }
  }

  // our own Try monad
  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] = {
      try {
        f(value)
      } catch {
        case e: Throwable => Failure(e)
      }
      //      f(value)
    }

  }

  case class Failure(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] =
      this
  }


  /*
    left-identity
    unit.flatMap(f) = f(x)
    Attempt(x).flatMap(f) = f(x) //Success
    Success(x).flatMap(f) = f(x) // proved.


    right-identity
    Attempt(x).flatMap(unit) = Attempt(x)
    Success(x).flat(x => Attempt(x)) = Attempt(x) = Success(x)
    Failure(e).flatMap(...) = Failure(e)

    associativity

    Attempt.flatMap(f).flatMap(g) = Attempt.flatMap(x => f(x).flatMap(g))
    Failure(e).flatMap(f).flatMap(g) = Failure(e)
    Failure(e).flatMap(x => f(x).flatMap(g)) = Failure(e)

    Success(v).flatMap(f).flatMap(g) =
      f(v).flatMap(g) OR Failure(e)

    Success(v).flatMap(x => f(x).flatMap(g)) =
      f(v).flatMap(g) OR Failure(e)


   */

  val attempt = Attempt {
    throw new RuntimeException
  }
  println(attempt)


  /*

    EXERCISE:
    1) implement a Lazy[T] monad = computation which will only be executed when it's needed
      unit/apply
      flatMap

    2) Monads = unit + flatMap
       Monads = unit + map + flatten

       Monad[t] {
        def flatMap[B](f : T => Monad[B]): Monad[B]
        def map[B](f: T => B): Monad[B] = ???
        def flatten(m: Monad[Monad[T]]): Monad[T] = ???
        }
   */

  class Lazy[+T](value: => T) {

    private lazy val internalValue = value

    def use: T = value

    def flatMap[B](f: (=> T) => Lazy[B]): Lazy[B] =
      f(internalValue)

    def map[B](f: T => B): Lazy[B] =
      Lazy(f(value))

  }

  object Lazy {
    def apply[T](nValue: => T): Lazy[T] =
      new Lazy(nValue)

  }


  val lazyInstance = Lazy {
    println("Today I don't feel like doing anything")
    42
  }


  val flatMappedInstance = lazyInstance.flatMap(x => Lazy({
    10 * x
  }))

  val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy({
    10 * x
  }))

  flatMappedInstance.use
  flatMappedInstance2.use
}



