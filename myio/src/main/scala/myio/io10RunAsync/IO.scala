/*
  In step 10 I added three async unsafeRun* methods: unsafeRunToFuture, unsafeRunOnComplete, unsafeRunAsync.
  All three accept an implicit ExecutionContext.
 */

package myio.io10RunAsync

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

sealed trait IO[+A] extends Product with Serializable {

  import IO._

  def unsafeRun(): A

  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)

  def map[B](f: A => B): IO[B] =
    flatMap(f andThen pure)

  def flatten[B](implicit ev: A <:< IO[B]): IO[B] =
    flatMap(a => a)

  // ----- impure sync unsafeRun* methods

  // runs on the current Thread returning Try[A]
  def unsafeRunToTry: Try[A] = Try { unsafeRun() }

  // runs on the current Thread returning Either[Throwable, A]
  def unsafeRunToEither: Either[Throwable, A] = unsafeRunToTry.toEither

  // ----- impure async unsafeRun* methods

  // returns a Future that runs the task eagerly on another thread
  def unsafeRunToFuture(implicit ec: ExecutionContext): Future[A] = Future { unsafeRun() }

  // runs the IO in a Runnable on the given ExecutionContext
  // and then executes the specified Try based callback
  def unsafeRunOnComplete(callback: Try[A] => Unit)(implicit ec: ExecutionContext): Unit =
    unsafeRunToFuture onComplete callback

  // runs the IO in a Runnable on the given ExecutionContext
  // and then executes the specified Either based callback
  def unsafeRunAsync(callback: Either[Throwable, A] => Unit)(implicit ec: ExecutionContext): Unit =
    unsafeRunOnComplete(tryy => callback(tryy.toEither))
}

object IO {

  private case class Pure[A](a: A) extends IO[A] {
    override def unsafeRun(): A = a
  }

  private case class Eval[A](thunk: () => A) extends IO[A] {
    override def unsafeRun(): A = thunk()
  }

  private case class FlatMap[A, B](src: IO[A], f: A => IO[B]) extends IO[B] {
    override def unsafeRun(): B = f(src.unsafeRun()).unsafeRun()
  }

  def pure[A](a: A): IO[A] = Pure(a)
  def now[A](a: A): IO[A]  = pure(a)

  def eval[A](a: => A): IO[A]  = Eval(() => a)
  def delay[A](a: => A): IO[A] = eval(a)
  def apply[A](a: => A): IO[A] = eval(a)
}
