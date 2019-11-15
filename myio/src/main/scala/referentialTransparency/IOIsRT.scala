package referentialTransparency

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO

import scala.concurrent.ExecutionContext.Implicits.global

/*
  see blogpost:
  https://www.reddit.com/r/scala/comments/3zofjl/why_is_future_totally_unusable/
 */
object IOIsRT extends util.App {

  val io1: IO[(Int, Int)] = {
    val atomicInt   = new AtomicInteger(0)
    val io: IO[Int] = IO { atomicInt.incrementAndGet }
    for {
      x <- io
      y <- io
    } yield (x, y)
  }

  io1.unsafeToFuture onComplete println // Success((1,2))

  // same as io1, but inlined
  val io2: IO[(Int, Int)] = {
    val atomicInt = new AtomicInteger(0)
    for {
      x <- IO { atomicInt.incrementAndGet }
      y <- IO { atomicInt.incrementAndGet }
    } yield (x, y)
  }

  io2.unsafeToFuture onComplete println // Success((1,2))    <-- same result

  Thread.sleep(200L)
  println("-----")
  println("The results are equal. --> IO IS referentially transparent.")
}
