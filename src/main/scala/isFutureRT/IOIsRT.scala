package isFutureRT

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.ExecutionContext.Implicits.global

import iomonad.IOApp11TraitWithAbstractRun.IO

/*
  see blogpost:
  https://www.reddit.com/r/scala/comments/3zofjl/why_is_future_totally_unusable/
 */
object IOIsRT extends App {

  println("\n-----")

  val task1: IO[(Int, Int)] = {
    val atomicInt = new AtomicInteger(0)
    val task: IO[Int] = IO { atomicInt.incrementAndGet }
    for {
      x <- task
      y <- task
    } yield (x, y)
  }

  // same as future1, but inlined
  val task2: IO[(Int, Int)] = {
    val atomicInt = new AtomicInteger(0)
    for {
      x <- IO { atomicInt.incrementAndGet }
      y <- IO { atomicInt.incrementAndGet }
    } yield (x, y)
  }

  task1 runAsync println     // Success((1,2))
  task2 runAsync println     // Success((1,2))    <-- same result

  Thread.sleep(200L)
  println("-----")
  println("The results are equal. --> IO IS referentially transparent.")
  println("-----\n")
}
