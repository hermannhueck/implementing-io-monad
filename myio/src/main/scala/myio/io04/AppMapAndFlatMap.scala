package myio.io04

object AppMapAndFlatMap extends util.App {

  // Program definition WITHOUT side effects. This program does nothing.
  // It is just a bunch of monadically composed functions which do not execute.
  //
  val program: IO[Unit] = for {
    _    <- IO(() => print(s"Welcome to Scala!  What's your name?   "))
    name <- IO(() => scala.io.StdIn.readLine)
    _    <- IO(() => println(s"Well hello, $name!"))
  } yield ()

  // Running the program's encapsulated Function0 produces the side effects.
  program.unsafeRun()
}
