package exercises

import scala.annotation.tailrec

abstract class MyStream[+A] {
  def isEmpty: Boolean

  def head: A

  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] // prepend operator

  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] //concatenator

  def foreach(f: A => Unit): Unit

  def map[B](f: A => B): MyStream[B]

  def flatMap[B](f: A => MyStream[B]): MyStream[B]

  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // takes the first n elements out of the stream

  def takeAsList(n: Int): List[A]

  /*
    [1 2 3].toList([]) =
    [2 3].toList([1])
    [3].toList([2 1])
    [].toList([3 2 1])

   */
  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else
      tail.toList(head :: acc)
}


object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException

  def tail: MyStream[Nothing] = throw new NoSuchElementException

  def #::[B >: Nothing](element: B): MyStream[B] = new ConsStream[B](element, this)

  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = ()

  def map[B](f: Nothing => B): MyStream[B] = this

  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  def take(n: Int): MyStream[Nothing] = this

  def takeAsList(n: Int): List[Nothing] = Nil
}

class ConsStream[A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false

  override val head: A = hd

  override lazy val tail: MyStream[A] = tl // call by need

  def #::[B >: A](element: B): MyStream[B] =
    new ConsStream[B](element, this)

  override def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = // the argument should by called by name to preserve the lazy evaluation in flatMap
    new ConsStream[B](head, tail ++ anotherStream) // still preserves lazy evaluation

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  def map[B](f: A => B): MyStream[B] =
    new ConsStream[B](f(head), tail.map(f)) // still preserves lazy evaluation

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    f(head) ++ tail.flatMap(f) // still preserves lazy evaluation same as ++

  def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head))
      new ConsStream[A](head, tail.filter(predicate)) // lazily evaluated
    else
      tail.filter(predicate) //will force the evaluation of the first element in the stream

  def take(n: Int): MyStream[A] = {
    if (n == 0)
      EmptyStream
    else
      new ConsStream[A](head, tail.take(n - 1)) // still preserves lazy evaluation, call by need
  }

  def takeAsList(n: Int): List[A] = take(n).toList()

}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new ConsStream[A](start, MyStream.from(generator(start))(generator))
}

object Fibonacci {
  def from(first: Int, second: Int): MyStream[Int] =
    new ConsStream[Int](first, new ConsStream[Int](second, Fibonacci.from(first + second, second + first + second)))
}

object StreamPlayground extends App {
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFromZero = 0 #:: naturals
  println(startFromZero.head)

  startFromZero.take(1000).foreach(println)

  //map, flatMap
  println(startFromZero.map(_ * 2).take(100).toList())
  println(startFromZero.flatMap(x => new ConsStream[Int](x, new ConsStream[Int](x + 1, EmptyStream))).take(10).toList())

  //println(startFromZero.filter(_ < 10).toList()) //filtering infinite stream is not logical
  println(startFromZero.filter(_ < 10).take(9).toList()) //will work with take

  // Exercises on streams
  // 1 - stream of fibonacci numbers
  // 2 - stream of prime numbers with an Eratosthenes' sieve

  /*
    [ 2 3 4 ..... ]
    filter out all numbers divisible by 2
    [ 3 4 7 9 11 .... ]
    filter out all numbers divisible by 3
    [ 2 3 5 7 11 13 17 ......]
    filter out all number divisible by 5
    ........
   */
  def fibonacci(first: BigInt, second: BigInt): MyStream[BigInt] =
    new ConsStream[BigInt](first, fibonacci(second, first + second))


  println(Fibonacci.from(0, 1).take(10).toList())
  println(fibonacci(0, 1).take(100).toList())

  // 2, 3, 4, 5, 6, 7
  def primes(numbers: MyStream[Int]): MyStream[Int] =
    if (numbers.isEmpty) numbers
    else {
      new ConsStream[Int](numbers.head,
        primes(numbers.tail.filter(_ % numbers.head != 0)))
    }

  val startFromTwo = MyStream.from(2)(_ + 1)

  println(primes(startFromTwo).take(10).toList())


}