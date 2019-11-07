package myio.io16

import myio.auth._

import scala.concurrent.ExecutionContext

object AppADTSuspend extends util.App {

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

  println("\n>>> IO#unsafeRunToTry:")
  printAuthTry(checkMaggie.unsafeRunToTry)

  println("\n>>> IO#unsafeRunToEither:")
  printAuthEither(checkMaggie.unsafeRunToEither)

  println("\n>>> IO#unsafeRunToFuture:")
  checkMaggie.unsafeRunToFuture onComplete authCallbackTry
  Thread sleep 500L

  println("\n>>> IO#unsafeRunOnComplete:")
  checkMaggie unsafeRunOnComplete authCallbackTry
  Thread sleep 500L

  println("\n>>> IO#unsafeRunAsync:")
  checkMaggie unsafeRunAsync authCallbackEither
  Thread sleep 500L

  println("-----")

  println("\n>>> IO.pure(...):")
  val io1 = IO.pure { println("immediate side effect"); 5 }
  //=> immediate side effect
  Thread sleep 2000L
  io1 foreach println
  //=> 5
  Thread sleep 2000L

  println("\n>>> IO.defer(IO.pure(...)):")
  val io2 = IO.defer { IO.pure { println("deferred side effect"); 5 } }
  Thread sleep 2000L
  io2 foreach println
  //=> deferred side effect
  //=> 5
  Thread sleep 2000L

}
