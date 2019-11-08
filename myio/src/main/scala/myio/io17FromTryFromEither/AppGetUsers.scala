package myio.io17FromTryFromEither

import myio.auth.User

import scala.util.Try

object AppGetUsers extends util.App {

  println("\n>>> IO#fromTry:")
  val tryy: Try[Seq[User]] = Try { User.getUsers }
  val io1: IO[Seq[User]]   = IO.fromTry(tryy)
  io1.unsafeRunToEither foreach println

  println("\n>>> IO#fromEither:")
  val either: Either[Throwable, Seq[User]] = tryy.toEither
  val io2: IO[Seq[User]]                   = IO.fromEither(either)
  io1.unsafeRunToEither foreach println
}
