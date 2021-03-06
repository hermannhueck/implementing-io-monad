/*
  Step 1: An IO program with side effects is IMPURE.
 */

package myio.io01Impure

object AppConsole extends util.App {

  // program definition WITH side effects
  def program(): Unit = {
    print("Welcome to Scala!  What's your name?   ")
    val name = scala.io.StdIn.readLine
    println(s"Hello, $name!")
  }

  program()
}
