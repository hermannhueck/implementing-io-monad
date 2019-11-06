package iomonad

/*
  In step 7 I added the subtype FlatMap to the ADT IO and expanded the 'run' method accordingly.
  The Method IO#flatMap just creates an instance of FlatMap.
  Now IO is trampolined and hence stack-safe.
 */
object IOApp07ADTFlatMap extends App {

  sealed trait IO[+A] extends Product with Serializable {

    import IO._

    def run(): A

    def flatMap[B](f: A => IO[B]): IO[B]            = FlatMap(this, f)
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
  program.run()
  program.run()

  println("-----\n")
}
