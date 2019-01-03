package iomonad

import scala.util.Try

/*
  The original IO#run() might throw an exception when run.
  In step 6 I added two additional synchronous run* methods which do not throw an exception:
  'runToTry' and 'runToEither'.
 */
object IOApp06RunSync extends App {

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



  println("\n-----")

  val program: IO[Unit] = for {
    welcome <- IO.pure("Welcome to Scala!")
    _       <- IO.eval { print(s"$welcome  What's your name?   ") }
    name    <- IO.eval { scala.io.StdIn.readLine }
    _       <- IO.eval { println(s"Well hello, $name!") }
  } yield ()

  // Running the program's encapsulated Function0 produces the side effects.
  val value: Unit = program.run()                                 // run sync, may throw an exception
  println(value)
  //=> ()

  val tryy: Try[Unit] = program.runToTry                          // run sync
  println(tryy)
  //=> Success(())

  val either: Either[Throwable, Unit] = program.runToEither       // run sync
  println(either)
  //=> Right(())

  Thread.sleep(200L)
  println("-----\n")
}
