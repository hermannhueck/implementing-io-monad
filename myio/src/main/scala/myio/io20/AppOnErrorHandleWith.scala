package myio.io20

object AppOnErrorHandleWith extends util.App {

  println("\n----- onErrorHandleWith:")

  val error1: IO[Int] = IO.raiseError[Int](new IllegalStateException("illegal state"))
  val error2: IO[Int] = IO.raiseError[Int](new RuntimeException("dummy"))

  val completeHandler: Throwable => IO[Int] = {
    case _: IllegalStateException =>
      IO.pure(-1)
    case t: Throwable =>
      IO.raiseError(t)
  }

  println(error1.onErrorHandleWith(completeHandler).unsafeRunToEither)
  println(error2.onErrorHandleWith(completeHandler).unsafeRunToEither)

  println("\n----- onErrorHandle:")

  val completeHandler2: Throwable => Int = {
    case _: IllegalStateException => -1
    case t: Throwable             => throw t
  }

  println(error1.onErrorHandle(completeHandler2).unsafeRunToEither)
  println(error2.onErrorHandle(completeHandler2).unsafeRunToEither)
}
