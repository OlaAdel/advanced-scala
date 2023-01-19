package lectures.part3concurrency

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success, Try}

object FuturesAndPromises extends App {

  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife // on ANOTHER thread
  }

  println(aFuture.value) //option[try[int]]

  println("waiting")
  //onComplete used for side effects
  aFuture.onComplete {
    case Failure(exception) => println("error", exception)
    case Success(value) => println("value", value)
  } //Some thread, don't make any assumption on which thread exactly

  Thread.sleep(3000)

  //mini social network

  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile): Unit =
      println(s"${this.name} poking ${anotherProfile.name}")

  }

  object SocialNetwork {
    //"database"
    val names = Map(
      "fb.id.mark" -> "Mark",
      "fb.id.bill" -> "Bill",
      "fb.id.dummy" -> "Dummy"
    )
    val friends = Map(
      "fb.id.mark" -> "fb.id.bill"
    )

    val random = new Random()

    //API

    def fetchProfile(id: String): Future[Profile] =
      Future {
        //fetching from the DB
        Thread.sleep(random.nextInt(300))
        Profile(id, names(id))
      }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  //client: mark to poke pill
  val mark = SocialNetwork.fetchProfile("fb.id.mark")
  mark.onComplete {
    case Failure(exception) => exception.printStackTrace()
    case Success(markProfile) =>
      val bill = SocialNetwork.fetchBestFriend(markProfile)
      bill.onComplete {
        case Success(billProfile) => markProfile.poke(billProfile)
        case Failure(exception) => exception.printStackTrace()
      }
  }

  Thread.sleep(1000)

  SocialNetwork.fetchProfile("fb.id.mark").flatMap {
    markProfile =>
      SocialNetwork.fetchBestFriend(markProfile)
        .map(billProfile => markProfile.poke(billProfile))
  }
  Thread.sleep(1000)

  for {
    markProfile <- SocialNetwork.fetchProfile("fb.id.mark")
    billProfile <- SocialNetwork.fetchBestFriend(markProfile)
  } yield markProfile.poke(billProfile) // Future[Unit]


  Thread.sleep(1000)

  // fallbacks
  SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.dummy", "Forever alone")
  }

  SocialNetwork.fetchProfile("unknown id").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.mark")
  }
  val fallbackResult = SocialNetwork.fetchProfile("unknown id").
    fallbackTo(SocialNetwork.fetchProfile("fb.id.mark"))


  //online banking app
  case class User(name: String)

  case class Transaction(send: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching from the DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, metchantName: String, cost: Double): String = {
      //fetch the user from the DB
      //create a transaction
      //WAIT for the transaction to finish
      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, metchantName, cost)
      } yield transaction.status
      val transactionStatus: String = Await.result(transactionStatusFuture, 2.seconds)
      transactionStatus
    }

  }

  println(BankingApp.purchase("Daniel", "iPhonw 12", "rock the jvm", 3000))


  // promises
  val promise = Promise[Int]() // "controller" over a future
  val future = promise.future


  // thread 1 - "consumer"
  future.onComplete {
    case Success(r) => println("[consumer] I've received " + r)
  }

  // thread 2 - "producer"
  val producer: Thread = new Thread(() => {
    println("[producer] crunching numbers.....")
    Thread.sleep(500)
    // "fulfilling the promise"|
    promise.success(42)
    println("[producer] done")
  })

  producer.start()
  Thread.sleep(1000)

  /*
    1) fulfill a future IMMEDIATELY with a value
    2) inSequence(fa, fb) => will run a future b if a is completed
    3) first(fa, fb) => new future with the first value of the two futures
    4) last(fa, fb) => new future with the last value
    5) retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]
   */


  //1) fulfill a future IMMEDIATELY with a value
  val immediateFuture = Future.successful(42)
  println(immediateFuture)

  //2) inSequence(fa, fb) => will run a future b if a is completed
  def inSequence[T](fa: Future[T], fb: Future[T]): Unit = {
    /*fa.onComplete {
      _ =>
        println(s"future A is completed, let's run B")
        fb
    }*/
    fa.flatMap(_ => fb)
  }

  val hiFuture = Future.successful(println("Hi"))
  val byeFuture = Future.successful(println("Bye"))

  inSequence(hiFuture, byeFuture)

  //3) first(fa, fb) => new future with the first value of the two futures
  val fastFuture = Future {
    Thread.sleep(200)
    42
  }
  val slowFuture = Future {
    Thread.sleep(500)
    100
  }

  def first[T](fa: Future[T], fb: Future[T]): Future[T] = {
    val promise = Promise[T]()

    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)

    promise.future
  }

  first(fastFuture, slowFuture).foreach(first => println(s"first $first"))
  Thread.sleep(1000)

  //    4) last(fa, fb) => new future with the last value
  def last[T](fa: Future[T], fb: Future[T]): Future[T] = {
    // 1 promise which both futures will try to complete
    // 2 promise which the LAST future will complete
    val bothPromise = Promise[T]
    val lastPromise = Promise[T]

    val checkAndComplete =
      (result: Try[T]) => {
        if (!bothPromise.tryComplete(result))
          lastPromise.complete(result)
      }
    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)
    lastPromise.future
  }

  last(fastFuture, slowFuture).foreach(last => println(s"last $last"))

  //5) retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]


  def retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T] = {
    val promise = Promise[T]
    action().onComplete {
      case Failure(_) => retryUntil(action, condition)
      case Success(value) =>
        if (condition(value))
          {
            println("condition has been met!")
            promise.success(value)
          }
        else {
          println(s"value didn't meet the condition $value")
          promise.completeWith(retryUntil(action, condition))
        }
    }
    promise.future
  }

  def retryUntil2[T](action: () => Future[T], condition: T => Boolean): Future[T] = {
    action()
      .filter(condition)
      .recoverWith {
        case _ => retryUntil(action, condition)
      }
  }

  val randomInt: () => Future[Int] = () => Future {
    Random.nextInt(10)
  }
  val condition: Int => Boolean = (x: Int) => x == 5

 // retryUntil(randomInt, condition).foreach(x => println(s"retry until $x"))

  retryUntil2(randomInt, condition).foreach(x => println(s"retry until $x"))

  Thread.sleep(1000)


}
