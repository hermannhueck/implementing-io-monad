package iomonad

/*
  Step 1: An IO program with side effects is IMPURE.
 */
object IOApp01Impure extends util.App {

  // program definition WITH side effects
  def program(): Unit = {
    print("Welcome to Scala!  What's your name?   ")
    val name = scala.io.StdIn.readLine
    println(s"Well hello, $name!")
  }

  program()
}
