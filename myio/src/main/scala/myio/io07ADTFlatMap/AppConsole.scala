package myio.io07ADTFlatMap

object AppConsole extends util.App {

  val program: IO[Unit] = for {
    welcome <- IO.pure("Welcome to Scala!")
    _       <- IO { print(s"$welcome  What's your name?   ") }
    name    <- IO { scala.io.StdIn.readLine }
    _       <- IO { println(s"Hello, $name!") }
  } yield ()

  // Running the program's encapsulated Function0 produces the side effects.
  program.unsafeRun()
}
