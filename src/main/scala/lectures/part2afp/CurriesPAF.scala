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

  //lifting = ETA-expansion
  val add4: Int => Int = curriedAdder(4) // doesn't work without defining the type annotation explicitly
  println(add4(6))

  // functions != methods
  def inc(x: Int) = x + 1

  List(1, 2, 3).map(inc) // List(1, 2, 3).map(x => inc(x))
  // compiler does ETA-expansion for us, it turns the method into a function

  // Partial function applications
  val add5 = curriedAdder(5) _ //turn it into Int => Int function

  // EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y

  def simpleAddMethod(x: Int, y: Int) = x + y

  def curriedAddMethod(x: Int)(y: Int) = x + y

  //add7: Int => Int = y => 7 + y
  // as many different implementations of add7 using the above


  val add7 = (x: Int) => simpleAddFunction(7, x)
  val add7_1 = simpleAddFunction.curried(7)
  val add7_2: Int => Int = simpleAddFunction(7, _) // alternative syntax for turning methods into functions

  val add7_3: Int => Int = simpleAddMethod(7, _: Int)

  val add7_4: Int => Int = curriedAddMethod(7)
  val add7_5 = curriedAddMethod(7) _
  val add7_6 = curriedAddMethod(7)(_) // PAF = alternative syntax


  //underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c

  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?") // x: String => concatenator("Hello", x, "how are you")
  println(insertName("Daniel"))

  val fillingInTheBlanks = concatenator("Hello", _: String, _: String)
  // (x, y) => concatenator("Hello", x, y)
  println(fillingInTheBlanks("Daniel", " Scala is awesome"))


  /*
    1. process a list of numbers and return their string representations with different formats
      Use the %4.2f, 8.6f and %12.12f with a curried formatter function
   */

  println("%8.6f".format(Math.PI))

  def curriedFormatter(format: String)(number: Double): String =
    format.format(number)


  val format42f = curriedFormatter("%4.2f")(_: Double)
  val format1212f = curriedFormatter("%12.12f") _
  val format86f = curriedFormatter("%8.6f")(_: Double)

  println(List(Math.PI, Math.E, 9.8).map(format42f))
  println(List(Math.PI, Math.E, 9.8).map(format1212f))
  println(List(Math.PI, Math.E, 9.8).map(format86f))
  println(List(Math.PI, Math.E, 9.8).map(curriedFormatter("%12.12f"))) // compiler does ETA-expansion for us

  /*
    2 difference between
      functions vs methods
      parameters: by-name vs 0-lambdas
   */

  def byName(n: => Int) = n + 1

  def byFunction(f: () => Int) = f() + 1


  def method: Int = 42

  def paranMethod(): Int = 42

  /*
  calling byName and byFunction
    - int
    - method
    - parenMethod
    - lambda
    - PAD
   */


  println(byName(5))
  println(byName(method))
  println(byName(paranMethod()))
  //println(byName(() => 1)) //not ok
  byName((() => 42) ())
  //byName(parentMethod _) //not ok

  //println(byFunction(5)) // not ok
  //println(byFunction(method)) //not ok!!!!, as its evaluated to its value 42, doesn't do ETA expansion, method is accessor method
  println(byFunction(paranMethod)) //compiler does ETA expansion for proper methods (those with parentheses)
  println(byFunction(() => 46))
  println(byFunction(paranMethod _)) // also works but warning - unnecessary


}
