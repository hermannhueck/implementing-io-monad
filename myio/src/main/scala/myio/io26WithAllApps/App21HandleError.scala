package myio.io26WithAllApps

object App21HandleError extends util.App {

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

  println("\n----- onErrorRecoverWith:")

  val partialHandler: PartialFunction[Throwable, IO[Int]] = {
    case _: IllegalStateException =>
      IO.pure(-1)
  }

  println(error1.onErrorRecoverWith(partialHandler).unsafeRunToEither)
  println(error2.onErrorRecoverWith(partialHandler).unsafeRunToEither)

  println("\n----- onErrorRecover:")

  val partialHandler2: PartialFunction[Throwable, IO[Int]] = {
    case _: IllegalStateException =>
      IO.pure(-1)
  }

  println(error1.onErrorRecoverWith(partialHandler2).unsafeRunToEither)
  println(error2.onErrorRecoverWith(partialHandler2).unsafeRunToEither)
}
