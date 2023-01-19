package playground

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
object playground extends App {

  println("I'm so ready to become a Scala Rockstar!")

  def timedFuture[T](future: Future[T]): Unit = {
    val start = System.currentTimeMillis()
    future.onComplete(_ => println(s"Future took ${System.currentTimeMillis() - start} ms"))
  }

  val first = Future {
    Thread.sleep(500)
    5
  }
  def x =
    first.map {
    throw new RuntimeException("heeey")
  }

  val y = first.flatMap{
    _ => Future.failed(new RuntimeException("timee"))
  }

  Thread.sleep(1000)

  println("here")
  //println(timedFuture(x))
  //timedFuture(y)


  Future.failed(throw new Exception("eee")).onComplete {
    case Failure(exception) => println("error")
    case Success(value) => println("vvv")
  }

}
