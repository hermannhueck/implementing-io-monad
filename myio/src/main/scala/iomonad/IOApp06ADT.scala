package iomonad

/*
  Step 6 converts case class IO into trait IO with the abstract method 'run'.
  IO is an ADT with the two subtypes 'Pure' and 'Eval'

  IO.pure creates a Pure instance instead of an IO instance.
  IO.now is an alias for pure.
  IO.eval creates a Eval instance instead of an IO instance.
  IO.delay is an alias for IO.eval.
  IO.apply is an alias for IO.eval.

  Having apply it is more natural to create new IO instances.
  We can just use IO { thunk } instead of IO.eval { thunk }
 */
object IOApp06ADT extends util.App {

  sealed trait IO[+A] extends Product with Serializable {

    import IO._

    def run(): A

    def flatMap[B](f: A => IO[B]): IO[B]            = IO { f(run()).run() }
    def map[B](f: A => B): IO[B]                    = flatMap(a => pure(f(a)))
    def flatten[B](implicit ev: A <:< IO[B]): IO[B] = flatMap(a => a)
  }

  object IO {

    private case class Pure[A](thunk: () => A) extends IO[A] {
      override def run(): A = thunk()
    }

    private case class Eval[A](thunk: () => A) extends IO[A] {
      override def run(): A = thunk()
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
  program.run()
  program.run()

}
