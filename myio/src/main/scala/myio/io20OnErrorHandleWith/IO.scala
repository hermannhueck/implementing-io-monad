/*
  Step 20 adds the error handling methods onErrorHandleWith and onErrorHandle.
  It also extends the Monad instance to an MonadError instance.
  And it adds an implicit class for MonadError like syntax extensions.
 */

package myio.io20OnErrorHandleWith

import cats.MonadError
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
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

  // Triggers async evaluation of this IO, executing the given function for the generated result.
  // WARNING: Will not be called if this IO is never completed or if it is completed with a failure.
  // Since this method executes asynchronously and does not produce a return value,
  // any non-fatal exceptions thrown will be reported to the ExecutionContext.
  def foreach(f: A => Unit)(implicit ec: ExecutionContext): Unit =
    unsafeRunAsync {
      case Left(ex)     => ec.reportFailure(ex)
      case Right(value) => f(value)
    }

  // Returns a failed projection of this task.
  //
  // The failed projection is a Task holding a value of type Throwable, emitting the error yielded by the source,
  // in case the source fails, otherwise if the source succeeds the result will fail with a NoSuchElementException.
  def failed: IO[Throwable] =
    this.flatMap {
      case Error(t) => IO.pure(t)
      case _        => IO.raiseError(new NoSuchElementException("failed"))
    }

  def onErrorHandleWith[AA >: A](f: Throwable => IO[AA]): IO[AA] =
    IO {
      this.unsafeRunToEither match {
        case Left(t)  => f(t)
        case Right(a) => IO.pure(a)
      }
    }.flatten

  def onErrorHandle[AA >: A](f: Throwable => AA): IO[AA] =
    onErrorHandleWith(t => IO.pure(f(t)))
}

object IO {

  private case class Pure[A](a: A) extends IO[A] {
    override def unsafeRun(): A = a
  }

  private case class Eval[A](thunk: () => A) extends IO[A] {
    override def unsafeRun(): A = thunk()
  }

  private case class Error[A](exception: Throwable) extends IO[A] {
    override def unsafeRun(): A = throw exception
  }

  private case class Suspend[A](thunk: () => IO[A]) extends IO[A] {
    override def unsafeRun(): A = thunk().unsafeRun()
  }

  private case class FlatMap[A, B](src: IO[A], f: A => IO[B]) extends IO[B] {
    override def unsafeRun(): B = f(src.unsafeRun()).unsafeRun()
  }

  private case class FromFuture[A](fa: Future[A]) extends IO[A] {
    override def unsafeRun(): A = Await.result(fa, Duration.Inf) // BLOCKING!!!
    // A solution of this problem would require a redesign of this simple IO Monod, which doesn't really support async computations.
  }

  def pure[A](a: A): IO[A] = Pure(a)
  def now[A](a: A): IO[A]  = pure(a)

  def raiseError[A](t: Throwable): IO[A] = Error[A](t)
  def fail[A](t: Throwable): IO[A]       = raiseError(t)

  def eval[A](a: => A): IO[A]  = Eval(() => a)
  def delay[A](a: => A): IO[A] = eval(a)
  def apply[A](a: => A): IO[A] = eval(a)

  def suspend[A](ioa: => IO[A]): IO[A] = Suspend(() => ioa)
  def defer[A](ioa: => IO[A]): IO[A]   = suspend(ioa)

  def fromTry[A](tryy: Try[A]): IO[A] =
    tryy.fold(IO.raiseError, IO.pure)

  def fromEither[A](either: Either[Throwable, A]): IO[A] =
    either.fold(IO.raiseError, IO.pure)

  def fromFuture[A](future: Future[A]): IO[A] = FromFuture(future)

  def deferFuture[A](future: => Future[A]): IO[A] =
    defer(IO.fromFuture(future))

  // MonadError instance defined in implicit scope
  implicit def ioMonad: MonadError[IO, Throwable] = new MonadError[IO, Throwable] {

    // Monad
    override def pure[A](value: A): IO[A]                              = IO.pure(value)
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B]        = fa flatMap f
    override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???

    // MonadError
    override def raiseError[A](e: Throwable): IO[A] = raiseError(e)

    override def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] =
      fa onErrorHandleWith f
  }

  implicit class syntax[A](ioa: IO[A]) { // provide corresponding methods of ApplicativeError/MonadError

    def handleErrorWith(f: Throwable => IO[A]): IO[A] = ioa onErrorHandleWith f
    def handleError(f: Throwable => A): IO[A]         = ioa onErrorHandle f
  }
}
