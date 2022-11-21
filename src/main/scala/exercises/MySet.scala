package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {
  /*
    Exercises - implement a functional set
   */
  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach(f: A => Unit): Unit

  def apply(elem: A): Boolean =
    contains(elem)

  /*
    EXERCISE
      - removing an element
      - intersection with another set
      - difference with another set
   */
  def -(elem: A): MySet[A] //remove element

  def &(anotherSet: MySet[A]): MySet[A] // intersection

  def --(anotherSet: MySet[A]): MySet[A] // difference

  // EXERCISE #3 - negation of a set
  def unary_! : MySet[A]

}

class EmptySet[T] extends MySet[T] {
  def contains(elem: T): Boolean = false

  def +(elem: T): MySet[T] = new ConsSet[T](elem, this)

  def ++(anotherSet: MySet[T]): MySet[T] = anotherSet

  def map[B](f: T => B): MySet[B] = new EmptySet[B]

  def flatMap[B](f: T => MySet[B]): MySet[B] = new EmptySet[B]

  def filter(predicate: T => Boolean): MySet[T] = this

  def foreach(f: T => Unit): Unit = ()

  def -(elem: T): MySet[T] = this

  def &(anotherSet: MySet[T]): MySet[T] = this

  def --(anotherSet: MySet[T]): MySet[T] = this

  def unary_! : MySet[T] = new PropertyBasedSet[T](_ => true)
}

class AllInclusiveSet[T] extends MySet[T] {
  def contains(elem: T): Boolean = true

  def +(elem: T): MySet[T] = this

  def ++(anotherSet: MySet[T]): MySet[T] = this

  // naturals
  // naturals..map(x => x % 3) => [0, 1, 2]
  def map[B](f: T => B): MySet[B] = ???

  def flatMap[B](f: T => MySet[B]): MySet[B] = ???

  def filter(predicate: T => Boolean): MySet[T] = ??? //property based set

  def foreach(f: T => Unit): Unit = ???

  def -(elem: T): MySet[T] = ???

  def &(anotherSet: MySet[T]): MySet[T] = filter(anotherSet)

  def --(anotherSet: MySet[T]): MySet[T] = filter(!anotherSet)

  def unary_! : MySet[T] = new EmptySet[T]
}

// all elements of type A which satisfy a property
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  def contains(elem: A): Boolean = property(elem)

  // { x in A | property(x) } + element
  def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)

  def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  def map[B](f: A => B): MySet[B] = politelyFail

  def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  def filter(predicate: A => Boolean): MySet[A] =
    new PropertyBasedSet[A](x => property(x) && predicate(x))

  def foreach(f: A => Unit): Unit = politelyFail

  def -(elem: A): MySet[A] =
    filter(x => x != elem)

  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  def unary_! : MySet[A] =
    new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException
}

class ConsSet[T](val head: T, val tail: MySet[T]) extends MySet[T] {


  def contains(elem: T): Boolean =
    if (elem == head) true
    else tail.contains(elem)

  def +(elem: T): MySet[T] =
    if (this contains elem) this
    else new ConsSet[T](elem, this)

  def ++(anotherSet: MySet[T]): MySet[T] =
    tail ++ anotherSet + head
  /* {
     @tailrec
     def setAccumulator(remainingAnotherSet: MySet[T], currentSet: MySet[T]): MySet[T] = {
       if (remainingAnotherSet.isInstanceOf[EmptySet[T]])
         currentSet
       else {
         val head = remainingAnotherSet.asInstanceOf[ConsSet[T]].head
         val tail = remainingAnotherSet.asInstanceOf[ConsSet[T]].tail
         setAccumulator(tail, currentSet + head)
       }
     }

     setAccumulator(anotherSet, this)
   }*/


  def map[B](f: T => B): MySet[B] =
    tail.map(f) + f(head)
  /*{
    @tailrec
    def setAccumulator(remaining: MySet[T], currentSet: MySet[B]): MySet[B] = {
      if (remaining.isInstanceOf[EmptySet[T]])
        currentSet
      else {
        val head = remaining.asInstanceOf[ConsSet[T]].head
        val tail = remaining.asInstanceOf[ConsSet[T]].tail
        setAccumulator(tail, currentSet + f(head))
      }
    }

    setAccumulator(this, new EmptySet[B])
  }*/

  def flatMap[B](f: T => MySet[B]): MySet[B] =
    f(head) ++ tail.flatMap(f)

  def filter(predicate: T => Boolean): MySet[T] = {
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) filteredTail + head
    else filteredTail
  }


  /* if (predicate(head))
     new ConsSet[T](head, tail.filter(predicate))
   else
     tail.filter(predicate)
 */

  def foreach(f: T => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  def -(elem: T): MySet[T] =
    if (elem == head)
      tail
    else
      tail - elem + head

  def &(anotherSet: MySet[T]): MySet[T] = // intersection == filter
    filter(anotherSet)


  /*
  if (anotherSet contains head)
    new ConsSet[T](head, anotherSet & tail)
  else
    anotherSet & tail
}*/

  def --(anotherSet: MySet[T]): MySet[T] =
    filter(!anotherSet)

  //filter(x => !anotherSet(x))

  /* if (!anotherSet.contains(head))
     new ConsSet[T](head, tail -- anotherSet)
   else
     tail -- anotherSet
*/

  def unary_! : MySet[T] = new PropertyBasedSet[T](x => !this.contains(x))

}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valueSeq: Seq[A], acc: MySet[A]): MySet[A] = {
      if (valueSeq.isEmpty) acc
      else buildSet(valueSeq.tail, acc + valueSeq.head)
    }

    buildSet(values, new EmptySet[A])
  }
}

object SetTest extends App {

  val set1 = MySet(1, 2, 3, 5, 6, 1)

  set1.foreach(println)
  val set2 = MySet(1, 2, 10, 11)

  println("++")
  (set1 ++ set2).foreach(println)

  println("map")
  (set1 ++ set2).map(_ => 1).foreach(println)

  println("flatmap")
  (set1 ++ set2).flatMap(x => MySet(x, x + 1)).foreach(println)


  println("filter")
  (set1 ++ set2).filter(x => x % 2 == 0).foreach(println)


  println("-")
  (MySet(1, 2, 3, 4) - 2).foreach(println)


  println("&")
  (MySet(1, 2, 3) & MySet(2, 3)).foreach(println)

  println("--")
  (MySet(1, 3, 5, 7, 9) -- MySet(2, 3, 4, 5)).foreach(println)

  val negative = !MySet(1, 2, 3, 4)
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5
  println(negativeEven5(5))
}