package myio.io15

import cats.Monad
import myio.compute.{factorial, fibonacci, sumOfRange}

object AppMonadInstance extends util.App {

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def sumF[F[_]: Monad](from: Int, to: Int): F[Int] =
    Monad[F].pure { sumOfRange(from, to) }

  def fibonacciF[F[_]: Monad](num: Int): F[BigInt] =
    Monad[F].pure { fibonacci(num) }

  def factorialF[F[_]: Monad](num: Int): F[BigInt] =
    Monad[F].pure { factorial(num) }

  def computeF[F[_]: Monad](from: Int, to: Int): F[BigInt] =
    for {
      x <- sumF(from, to)
      y <- fibonacciF(x)
      z <- factorialF(y.intValue)
    } yield z

  def computeWithIO(): Unit = {

    import scala.concurrent.ExecutionContext.Implicits.global

    println(">> reify F[] with IO")
    val io: IO[BigInt] = computeF[IO](1, 4)

    io foreach { result =>
      println(s"result = $result")
    }
    //=> 6227020800

    Thread sleep 500L
  }

  def computeWithId(): Unit = {

    import cats.Id

    println(">> reify F[] with cats.Id")
    val result: Id[BigInt] = computeF[Id](1, 4)

    println(s"result = $result")
    //=> 6227020800

    Thread sleep 500L
  }

  def computeWithOption(): Unit = {

    import cats.instances.option._

    println(">> reify F[] with Option")
    val maybeResult: Option[BigInt] = computeF[Option](1, 4)

    maybeResult foreach { result =>
      println(s"result = $result")
    }
    //=> 6227020800

    Thread sleep 500L
  }

  def computeWithFuture(): Unit = {

    import cats.instances.future._

    import scala.concurrent.{ExecutionContext, Future}
    import ExecutionContext.Implicits.global

    println(">> reify F[] with Future")
    val future: Future[BigInt] = computeF[Future](1, 4)

    future foreach { result =>
      println(s"result = $result")
    }
    //=> 6227020800

    Thread sleep 500L
  }

  computeWithIO()
  computeWithId()
  computeWithOption()
  computeWithFuture()
}
