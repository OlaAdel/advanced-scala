package lectures.part2afp

object CurriesPAF extends App {

  // curried functions
  val superAdder: Int => (Int => Int) =
    x => y => x + y

  val add3 = superAdder(3)
  println(add3(5))

  println(superAdder(3)(5)) //curried function

  // curried methods
  // METHOD!
  def curriedAdder(x: Int)(y: Int): Int =
    x + y

  //lifting
  val add4: Int => Int = curriedAdder(4)
  println(add4(6))


}
