package lectures.part2afp

object PartialFunctions extends App {

  //any int will return a result
  val aFunction = (x: Int) => x + 1 //Function1[Int,Int] === Int => Int)

  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 599
  }

  // {1, 2, 5} => Int

  //sweeter shorthand notation for aNicerFussyFunction, but
  //partial function literal
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 599
  } // partial function value

  println(aPartialFunction(2))
  //println(aPartialFunction(5898)) //throwing MatchError


  // PF utilities

  println(aPartialFunction.isDefinedAt(67))

  // can be lifted to total function that returning option
  val lifted = aPartialFunction.lift // Int => Option[Int]
  println(lifted(2))
  println(lifted(666))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }
  println(pfChain(2))
  println(pfChain(45))

  // PF extend normal functions

  //that's why I can supply partial function to Int => Int, as it's a subtype of it
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accept partial functions as well
  val aMappedList = List(1, 2, 3).map {
    case 1 => 43
    case 2 => 78
    case 3 => 1000
  }

  println(aMappedList)

  /*
    Note: PF can only have ONE parameter
   */

  /*
    Exercises
      1 - construct a PF instance yourself (anonymous class)
      2 - dumb chat-bot as a PF
   */



  val PFInstance = new PartialFunction[Int, String] {
    override def isDefinedAt(x: Int): Boolean =
      List(1, 2, 3).contains(x)

    override def apply(x: Int): String = x match {
      case 1 => "The ONE"
      case 2 => "Double"
      case 3 => "Triple"
    }
  }

  println(PFInstance(2))
  println(PFInstance.isDefinedAt(5))

  val chatbot: PartialFunction[String, String] = {
    case "hello" => "Hi, my name is HAL9000"
    case "bye"  => "once you start talking to me, there is no return, human!"
  }

  scala.io.Source.stdin.getLines().map(chatbot).foreach(println)

}
