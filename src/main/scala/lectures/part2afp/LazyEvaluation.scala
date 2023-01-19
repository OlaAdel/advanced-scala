package lectures.part2afp

object LazyEvaluation extends App {

  lazy val x: Int = {
    println("hello")
    42
  }
  println(x) // x evaluated once
  println(x)

  //example of implications:
  // side effecs
  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition

  println(if (simpleCondition && lazyCondition) "yes" else "no") //side effect is not printed out(as lazyCondition is never evaluated)

  // in conjunction with call by name

  //n is going to be evaluated 3 times
  def byNameMethod(n: => Int): Int = {
    lazy val t = n // only evaluated once
    t + t + t + 1
  }

  def retrieveMagicValue = {
    // side effect or a long computation
    println("waiting")
    Thread.sleep(1000)
    42
  }


  //use lazy val instead of the parameter n

  println(byNameMethod(retrieveMagicValue))


  //filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30)
  val gt20Lazy = lt30lazy.withFilter(greaterThan20)
  println
  gt20Lazy.foreach(println)


  // for-comprehensions use withFilter with guards

  for {
    a <- List(1, 2, 3) if a % 2 == 0 // use lazy vals!
  } yield a + 1
  List(1, 2, 3).withFilter(_ % 2 == 0).map(_ + 1) // List[Int]

  /*
    Exercise: implement a lazily evaluated, singly, linked STREAM of elements
    MyStream.from(1)(x => x + 1) = stream of natural numbers (infinite stream)

    naturals.take(100).foreach(println) // lazily evaluated stream of the first 100 naturals (finite steam)
    naturals.foreach(println) // will crash - infinite
    naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
   */

}
