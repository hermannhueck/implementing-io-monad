package littleMonadDemo.ch03UsingCatsMonad

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success

import cats._
import cats.implicits._

object UsingCatsMonad extends util.App {

  def compute[F[_]: Monad](fInt1: F[Int], fInt2: F[Int]) =
    for {
      i1 <- fInt1
      i2 <- fInt2
    } yield (i1, i2)

  def compute2[F[_]: Monad](fInt1: F[Int], fInt2: F[Int]) =
    fInt1.flatMap { i1 =>
      fInt2.map { i2 =>
        (i1, i2)
      }
    }

  println("----- List:")

  val l1 = List(1, 2, 3)
  val l2 = List(1, 20, 30, 40)

  val lResult = compute(l1, l2)
  println(lResult)

  val lResult2 = compute2(l1, l2)
  println(lResult2)

  println("----- Option:")

  val o1 = Option(1)
  val o2 = Option(10)

  val oResult = compute(o1, o2)
  println(oResult)

  println("----- Future:")

  implicit val ec = ExecutionContext.global

  val f1 = Future(1)
  val f2 = Future(10)

  val fResult = compute(f1, f2)
  val res     = Await.result(fResult, 3.seconds) // BLOCKING !!!
  println(res)

  fResult onComplete (
      `tryä` =>
        println(
          `tryä`.fold(
            throwable => throwable.getMessage,
            value => value.toString
          )
        )
    )

  fResult onComplete {
    case Failure(exception) => println(exception.getMessage())
    case Success(value)     => println(value)
  }

  Thread sleep 500L
}
