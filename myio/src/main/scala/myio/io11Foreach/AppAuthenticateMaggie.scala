package myio.io11Foreach

import myio.auth._

import scala.concurrent.ExecutionContext
import scala.util.Try

object AppAuthenticateMaggie extends util.App {

  import Password._
  import User._

  def authenticate(username: String, password: String): IO[Boolean] =
    for {
      optUser <- IO(getUsers) map { users =>
                  users.find(_.name == username)
                }
      isAuthenticated <- IO(getPasswords) map { passwords =>
                          optUser.isDefined && passwords.contains(
                            Password(optUser.get.id, password)
                          )
                        }
    } yield isAuthenticated

  IO(getUsers).unsafeRun() foreach println
  println

  IO(getPasswords).unsafeRun() foreach println

  println("\n>>> IO#unsafeRun: authenticate:")
  println(authenticate("maggie", "maggie-pw").unsafeRun())
  println(authenticate("maggieXXX", "maggie-pw").unsafeRun())
  println(authenticate("maggie", "maggie-pwXXX").unsafeRun())

  implicit val ec: ExecutionContext = ExecutionContext.global

  IO(getUsers) foreach { users =>
    users foreach println
  }
  Thread sleep 500L
  println

  IO(getPasswords) foreach { users =>
    users foreach println
  }
  Thread sleep 500L

  println("\n>>> IO#foreach: authenticate:")
  authenticate("maggie", "maggie-pw") foreach println //=> true
  Thread sleep 200L
  authenticate("maggieXXX", "maggie-pw") foreach println //=> false
  Thread sleep 200L
  authenticate("maggie", "maggie-pwXXX") foreach println //=> false
  Thread sleep 200L

  val checkMaggie: IO[Boolean] = authenticate("maggie", "maggie-pw")

  println("\n>>> IO#unsafeRun:")
  val value: Boolean = checkMaggie.unsafeRun()
  println(value)
  //=> true, may throw an Exception

  println("\n>>> IO#unsafeRunToTry:")
  val tryy: Try[Boolean] = checkMaggie.unsafeRunToTry
  println(tryy)
  //=> Success(true)

  println("\n>>> IO#unsafeRunToEither:")
  val either: Either[Throwable, Boolean] = checkMaggie.unsafeRunToEither
  println(either)
  //=> Right(true)

  println("\n>>> IO#unsafeRunToFuture:")
  checkMaggie.unsafeRunToFuture onComplete authCallbackTry
  Thread sleep 500L

  println("\n>>> IO#unsafeRunOnComplete:")
  checkMaggie unsafeRunOnComplete authCallbackTry
  Thread sleep 500L

  println("\n>>> IO#unsafeRunAsync:")
  checkMaggie unsafeRunAsync authCallbackEither
  Thread sleep 500L
}
