/*
  In step 7 I added the subtype FlatMap to the ADT IO and expanded the 'unsafeRun' method accordingly.
  The Method IO#flatMap just creates an instance of FlatMap.
  Now IO is trampolined and hence stack-safe.
 */

package myio.io07

sealed trait IO[+A] extends Product with Serializable {

  import IO._

  def unsafeRun(): A

  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)

  def map[B](f: A => B): IO[B] =
    // flatMap(a => pure(f(a))) // is equivalent to:
    flatMap(f andThen pure)

  def flatten[B](implicit ev: A <:< IO[B]): IO[B] =
    flatMap(a => a)
}

object IO {

  private case class Pure[A](thunk: () => A) extends IO[A] {
    override def unsafeRun(): A = thunk()
  }

  private case class Eval[A](thunk: () => A) extends IO[A] {
    override def unsafeRun(): A = thunk()
  }

  private case class FlatMap[A, B](src: IO[A], f: A => IO[B]) extends IO[B] {
    override def unsafeRun(): B = f(src.unsafeRun()).unsafeRun()
  }

  def pure[A](a: A): IO[A] = Pure(() => a)
  def now[A](a: A): IO[A]  = pure(a)

  def eval[A](a: => A): IO[A]  = Eval(() => a)
  def delay[A](a: => A): IO[A] = eval(a)
  def apply[A](a: => A): IO[A] = eval(a)
}
