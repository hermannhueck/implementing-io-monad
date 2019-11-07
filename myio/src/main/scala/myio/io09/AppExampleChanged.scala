package myio.io09

import myio.auth.{Password, User}

import scala.util.Try

object AppExampleChanged extends util.App {

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

}
