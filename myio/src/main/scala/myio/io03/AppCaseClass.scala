package myio.io03

object AppCaseClass extends util.App {

  // Program definition WITHOUT side effects. This program does nothing.
  // It is just a Function0[Unit] wrapped in IO.
  //
  val program: IO[Unit] = IO(() => {
    print("Welcome to Scala!  What's your name?   ")
    val name = scala.io.StdIn.readLine
    println(s"Well hello, $name!")
  })

  // Running the program's encapsulated Function0 produces the side effects.
  program.unsafeRun()
}
