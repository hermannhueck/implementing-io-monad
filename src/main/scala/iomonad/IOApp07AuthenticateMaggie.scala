package iomonad

import iomonad.auth._

import scala.util.Try

/*
  Before adding asynchronous run* methods in step 8 I use another example program.
  The previous interactive program is not very suitable to demonstrate asynchrony.

  The 'authenticate' method accesses the files 'users.txt' and 'passwords.txt'
  to check a username and a password and returns true if the specified username
  exists in 'users.txt' and the specified password matches with the user's passord in 'passwords.txt'.
 */
object IOApp07AuthenticateMaggie extends App {

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

  println("-----\n")
}
