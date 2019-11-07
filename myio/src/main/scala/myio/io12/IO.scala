/*
  Step 12 implements 'IO.raiseError'.
  It adds sub type Error to the ADT IO.
 */

package myio.io12

import scala.util.Try
import scala.concurrent.{ExecutionContext, Future}

sealed trait IO[+A] extends Product with Serializable {

  import IO._

  protected def unsafeRun(): A

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

  // Triggers async evaluation of this IO, executing the given function for the generated result.
  // WARNING: Will not be called if this IO is never completed or if it is completed with a failure.
  // Since this method executes asynchronously and does not produce a return value,
  // any non-fatal exceptions thrown will be reported to the ExecutionContext.
  def foreach(f: A => Unit)(implicit ec: ExecutionContext): Unit =
    unsafeRunAsync {
      case Left(ex)     => ec.reportFailure(ex)
      case Right(value) => f(value)
    }
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

  private case class Error[A](exception: Throwable) extends IO[A] {
    override def unsafeRun(): A = throw exception
  }

  def pure[A](a: A): IO[A] = Pure(() => a)
  def now[A](a: A): IO[A]  = pure(a)

  def raiseError[A](t: Throwable): IO[A] = Error[A](t)
  def fail[A](t: Throwable): IO[A]       = raiseError(t)

  def eval[A](a: => A): IO[A]  = Eval(() => a)
  def delay[A](a: => A): IO[A] = eval(a)
  def apply[A](a: => A): IO[A] = eval(a)
}
