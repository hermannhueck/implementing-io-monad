package iomonad

import iomonad.auth._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/*
  In step 8 I added three async run* methods: runToFuture, runOnComplete, runAsync.
  All three accept an implicit ExecutionContext.
 */
object IOApp08RunAsync extends App {

  case class IO[A](run: () => A) {

    import IO._

    def flatMap[B](f: A => IO[B]): IO[B] = IO { () => f(run()).run() }
    def map[B](f: A => B): IO[B] = flatMap(a => pure(f(a)))
    def flatten[B](implicit ev: A <:< IO[B]): IO[B] = flatMap(a => a)

    // ----- impure sync run* methods

    // runs on the current Thread returning Try[A]
    def runToTry: Try[A] = Try { run() }

    // runs on the current Thread returning Either[Throwable, A]
    def runToEither: Either[Throwable, A] = runToTry.toEither

    // ----- impure async run* methods

    // returns a Future that runs the task eagerly on another thread
    def runToFuture(implicit ec: ExecutionContext): Future[A] = Future { run() }

    // takes a Try based callback
    def runOnComplete(callback: Try[A] => Unit)(implicit ec: ExecutionContext): Unit =
      runToFuture onComplete callback

    // takes a Either based callback
    def runAsync(callback: Either[Throwable, A] => Unit)(implicit ec: ExecutionContext): Unit =
      runOnComplete(tryy => callback(tryy.toEither))
  }

  object IO {
    def pure[A](value: A): IO[A] = IO { () => value }
    def eval[A](thunk: => A): IO[A] = IO { () => thunk }
  }



  import User._
  import Password._

  def authenticate(username: String, password: String): IO[Boolean] =
    for {
      optUser <- IO.eval(getUsers) map { users =>
        users.find(_.name == username)
      }
      isAuthenticated <- IO.eval(getPasswords) map { passwords =>
        optUser.isDefined && passwords.contains(Password(optUser.get.id, password))
      }
    } yield isAuthenticated



  println("\n-----")

  IO.eval(getUsers).run() foreach println
  println("-----")

  IO.eval(getPasswords).run() foreach println
  println("-----")

  println("\n>>> IO#run: authenticate:")
  println(authenticate("maggie", "maggie-pw").run())
  println(authenticate("maggieXXX", "maggie-pw").run())
  println(authenticate("maggie", "maggie-pwXXX").run())


  val checkMaggie: IO[Boolean] = authenticate("maggie", "maggie-pw")

  println("\n>>> IO#run:")
  val value: Boolean = checkMaggie.run()
  println(value)
  //=> true, may throw an Exception

  println("\n>>> IO#runToTry:")
  val tryy: Try[Boolean] = checkMaggie.runToTry
  println(tryy)
  //=> Success(true)

  println("\n>>> IO#runToEither:")
  val either: Either[Throwable, Boolean] = checkMaggie.runToEither
  println(either)
  //=> Right(true)

  implicit val ec: ExecutionContext = ExecutionContext.global

  println("\n>>> IO#runToFuture:")
  checkMaggie.runToFuture onComplete authCallbackTry
  Thread sleep 500L

  println("\n>>> IO#runOnComplete:")
  checkMaggie runOnComplete authCallbackTry
  Thread sleep 500L

  println("\n>>> IO#runAsync:")
  checkMaggie runAsync authCallbackEither
  Thread sleep 500L

  println("-----\n")
}
