/*
  Step 6 converts case class IO into trait IO with the abstract method 'unsafeRun'.
  IO is an ADT with the two subtypes 'Pure' and 'Eval'

  IO.pure creates a Pure instance instead of an IO instance.
  IO.now is an alias for pure.
  IO.eval creates a Eval instance instead of an IO instance.
  IO.delay is an alias for IO.eval.
  IO.apply is an alias for IO.eval.

  Having apply it is more natural to create new IO instances.
  We can just use IO { thunk } instead of IO.eval { thunk }
 */

package myio.io06ADT

sealed trait IO[+A] extends Product with Serializable {

  import IO._

  def unsafeRun(): A

  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(f(unsafeRun()).unsafeRun())

  def map[B](f: A => B): IO[B] =
    // flatMap(a => pure(f(a))) // is equivalent to:
    flatMap(f andThen pure)

  def flatten[B](implicit ev: A <:< IO[B]): IO[B] =
    flatMap(a => a)
}

object IO {

  private final case class Pure[A](a: A) extends IO[A] {
    override def unsafeRun(): A = a
  }

  private final case class Eval[A](thunk: () => A) extends IO[A] {
    override def unsafeRun(): A = thunk()
  }

  def pure[A](a: A): IO[A] = Pure(a)
  def now[A](a: A): IO[A]  = pure(a)

  def eval[A](a: => A): IO[A]  = Eval(() => a)
  def delay[A](a: => A): IO[A] = eval(a)
  def apply[A](a: => A): IO[A] = eval(a)
}
