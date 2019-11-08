package myio.io23AttemptEnsure

object AppOddEven extends util.App {

  val r = scala.util.Random

  def even(num: Int): Boolean = num % 2 == 0
  def odd(num: Int): Boolean  = !even(num)

  val io = IO {
    val num = r.nextInt(100)
    if (even(num))
      num
    else
      throw new IllegalStateException("odd number")
  }

  println("\n----- attempt:")

  val outer: Either[Throwable, Either[Throwable, Int]] = io.attempt.unsafeRunToEither
  println(outer)
  val inner = outer.flatMap(x => x) // 2.12 or ... in 2.13 .flatten
  println(inner)

  Thread sleep 500L
  println("\n----- ensure:")

  println(
    io.ensure(new IllegalStateException("not divisable by 10"))(_ % 10 == 0).unsafeRunToEither
  )

  Thread sleep 500L
  println("\n----- ensureOr:")

  println(
    io.ensureOr(num => new IllegalStateException(s"$num not divisable by 10"))(_ % 10 == 0)
      .unsafeRunToEither
  )

  Thread sleep 500L
}
