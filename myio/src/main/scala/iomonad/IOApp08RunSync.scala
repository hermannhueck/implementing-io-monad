package iomonad

import scala.util.Try

/*
  The original IO#run() might throw an exception when run.
  In step 8 I added two additional synchronous run* methods which do not throw an exception:
  'runToTry' and 'runToEither'.
 */
object IOApp08RunSync extends util.App {

  sealed trait IO[+A] extends Product with Serializable {

    import IO._

    def run(): A

    def flatMap[B](f: A => IO[B]): IO[B]            = FlatMap(this, f)
    def map[B](f: A => B): IO[B]                    = flatMap(a => pure(f(a)))
    def flatten[B](implicit ev: A <:< IO[B]): IO[B] = flatMap(a => a)

    // ----- impure sync run* methods

    // runs on the current Thread returning Try[A]
    def runToTry: Try[A] = Try { run() }

    // runs on the current Thread returning Either[Throwable, A]
    def runToEither: Either[Throwable, A] = runToTry.toEither
  }

  object IO {

    private case class Pure[A](thunk: () => A) extends IO[A] {
      override def run(): A = thunk()
    }

    private case class Eval[A](thunk: () => A) extends IO[A] {
      override def run(): A = thunk()
    }

    private case class FlatMap[A, B](src: IO[A], f: A => IO[B]) extends IO[B] {
      override def run(): B = f(src.run()).run()
    }

    def pure[A](a: A): IO[A] = Pure { () =>
      a
    }
    def now[A](a: A): IO[A] = pure(a)

    def eval[A](a: => A): IO[A] = Eval { () =>
      a
    }
    def delay[A](a: => A): IO[A] = eval(a)
    def apply[A](a: => A): IO[A] = eval(a)
  }

  println("\n-----")

  val program: IO[Unit] = for {
    welcome <- IO.pure("Welcome to Scala!")
    _       <- IO { print(s"$welcome  What's your name?   ") }
    name    <- IO { scala.io.StdIn.readLine }
    _       <- IO { println(s"Well hello, $name!") }
  } yield ()

  // Running the program's encapsulated Function0 produces the side effects.
  val value: Unit = program.run() // run sync, may throw an exception
  println(value)
  //=> ()

  val tryy: Try[Unit] = program.runToTry // run sync
  println(tryy)
  //=> Success(())

  val either: Either[Throwable, Unit] = program.runToEither // run sync
  println(either)
  //=> Right(())

  Thread.sleep(200L)
}
