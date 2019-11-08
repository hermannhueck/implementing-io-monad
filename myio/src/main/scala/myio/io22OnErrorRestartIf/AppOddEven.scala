package myio.io22OnErrorRestartIf

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

  println("\n----- onErrorRestartIf:")

  io.onErrorRestartIf {
    case _: IllegalStateException => true
    case _                        => false
  }.unsafeRunToEither foreach println

  Thread sleep 500L
  println("\n----- onErrorRestart:")

  io.onErrorRestart(3).unsafeRunToEither foreach println

  Thread sleep 500L
  println("\n----- onErrorFallbackTo:")

  io.onErrorFallbackTo(IO.pure(0)).unsafeRunToEither foreach println

  Thread sleep 500L
}
