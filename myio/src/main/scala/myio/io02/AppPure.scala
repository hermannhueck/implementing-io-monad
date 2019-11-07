/*
  Step 2: Making the program a Function0[A] converts the program to a value,
  a PURE description of the side effects which are not yet performed.
  Side effects are produced when the program is invoked, not earlier.
 */

package myio.io02

object AppPure extends util.App {

  // Program definition WITHOUT side effects. This program does nothing.
  // It is just a Function0[Unit].
  //
  val program: () => Unit = //  () => Unit  is syntactic sugar for:  Function0[Unit]
    () => {
      print("Welcome to Scala!  What's your name?   ")
      val name = scala.io.StdIn.readLine
      println(s"Well hello, $name!")
    }

  // Running the program produces the side effects.
  program()
}
