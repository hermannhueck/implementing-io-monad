package myio.io14ForComprehension

import myio.compute.{factorial, fibonacci, sumOfRange}

import scala.concurrent.ExecutionContext

object AppCompute extends util.App {

  def sumIO(from: Int, to: Int): IO[Int] =
    IO { sumOfRange(from, to) }

  def fibonacciIO(num: Int): IO[BigInt] =
    IO { fibonacci(num) }

  def factorialIO(num: Int): IO[BigInt] =
    IO { factorial(num) }

  def computeIO(from: Int, to: Int): IO[BigInt] =
    for {
      x <- sumIO(from, to)
      y <- fibonacciIO(x)
      z <- factorialIO(y.intValue)
    } yield z

  val io: IO[BigInt] = computeIO(1, 4)

  implicit val ec: ExecutionContext = ExecutionContext.global
  io foreach { result =>
    println(s"result = $result")
  }
  //=> 6227020800

  Thread sleep 500L
}
