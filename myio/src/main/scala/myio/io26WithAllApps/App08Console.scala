package myio.io26WithAllApps

import scala.util.Try

object App08Console extends util.App {

  val program: IO[Unit] = for {
    welcome <- IO.pure("Welcome to Scala!")
    _       <- IO { print(s"$welcome  What's your name?   ") }
    name    <- IO { scala.io.StdIn.readLine }
    _       <- IO { println(s"Hello, $name!") }
  } yield ()

  // Running the program's encapsulated Function0 produces the side effects.
  val value: Unit = program.unsafeRun() // run sync, may throw an exception
  println(value)
  //=> ()

  val tryy: Try[Unit] = program.unsafeRunToTry // run sync
  println(tryy)
  //=> Success(())

  val either: Either[Throwable, Unit] = program.unsafeRunToEither // run sync
  println(either)
  //=> Right(())

  Thread.sleep(200L)
}
