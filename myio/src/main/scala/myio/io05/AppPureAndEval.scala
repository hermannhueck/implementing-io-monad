package myio.io05

object AppPureAndEval extends util.App {

  val program: IO[Unit] = for {
    welcome <- IO.pure("Welcome to Scala!")
    _       <- IO.eval { print(s"$welcome  What's your name?   ") }
    name    <- IO.eval { scala.io.StdIn.readLine }
    _       <- IO.eval { println(s"Hello, $name!") }
  } yield ()

  // Running the program's encapsulated Function0 produces the side effects.
  program.unsafeRun()
}
