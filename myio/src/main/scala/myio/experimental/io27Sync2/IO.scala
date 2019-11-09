/*
  Step 27 provides an instance of type class Async
 */

package myio.experimental.io27Sync2

import cats.effect.{ExitCase, Sync}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.Success
import scala.util.Failure

sealed trait IO[+A] extends Product with Serializable {

  import IO._

  protected def unsafeRun(cb: Try[A] => Unit): Unit

  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)

  def map[B](f: A => B): IO[B] =
    flatMap(f andThen pure)

  def flatten[B](implicit ev: A <:< IO[B]): IO[B] =
    flatMap(a => a)

  // ----- impure sync unsafeRun* methods

  // runs on the current Thread returning Try[A]
  def unsafeRunToTry(cb: Try[A] => Unit): Try[Unit] = Try { unsafeRun(cb) }

  // runs on the current Thread returning Either[Throwable, A]
  def unsafeRunToEither(cb: Try[A] => Unit): Either[Throwable, Unit] = unsafeRunToTry(cb).toEither

  // ----- impure async unsafeRun* methods

  // returns a Future that runs the task eagerly on another thread
  def unsafeRunToFuture(cb: Try[A] => Unit)(implicit ec: ExecutionContext): Future[Unit] =
    Future { unsafeRun(cb) }

  // runs the IO in a Runnable on the given ExecutionContext
  // and then executes the specified Try based callback
  def unsafeRunOnComplete(cb: Try[A] => Unit)(implicit ec: ExecutionContext): Unit =
    unsafeRunToFuture(cb) onComplete {
      case Success(_) => ()
      case Failure(t) => IO.raiseError(t)
    }

  // runs the IO in a Runnable on the given ExecutionContext
  // and then executes the specified Either based callback
  def unsafeRunAsync(cb: Either[Throwable, A] => Unit)(implicit ec: ExecutionContext): Unit =
    unsafeRunOnComplete(tryy => cb(tryy.toEither))

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

  private[this] case class ErrorHandler[T](fe: Throwable => IO[T]) extends (T => IO[T]) {

    def recover(e: Throwable): IO[T] = fe(e)
    def apply(t: T): IO[T]           = IO.pure(t)
  }

  def onErrorHandleWith[AA >: A](fe: Throwable => IO[AA]): IO[AA] =
    FlatMap(this, ErrorHandler(fe))

  def onErrorHandle[AA >: A](f: Throwable => AA): IO[AA] =
    onErrorHandleWith(t => IO.pure(f(t)))

  def onErrorRecoverWith[AA >: A](pf: PartialFunction[Throwable, IO[AA]]): IO[AA] =
    onErrorHandleWith { t =>
      pf.applyOrElse(t, raiseError)
    }

  def onErrorRecover[AA >: A](pf: PartialFunction[Throwable, AA]): IO[AA] =
    onErrorHandle { t =>
      pf.applyOrElse(t, throw _: Throwable)
    }

  def onErrorRestartIf(p: Throwable => Boolean): IO[A] =
    onErrorHandleWith { t =>
      if (p(t))
        onErrorRestartIf(p)
      else
        IO.raiseError(t)
    }

  def onErrorRestart(maxRetries: Long): IO[A] =
    onErrorHandleWith { t =>
      if (maxRetries > 0)
        onErrorRestart(maxRetries - 1)
      else
        raiseError(t)
    }

  def onErrorFallbackTo[B >: A](that: IO[B]): IO[B] =
    onErrorHandleWith(_ => that)

  def attempt[AA >: A]: IO[Either[Throwable, AA]] =
    this
      .map { t =>
        Right(t): Either[Throwable, A]
      }
      .onErrorHandleWith { e =>
        IO.pure(Left(e))
      }

  // Turns a successful value into an error if it does not satisfy a given predicate. See cats.MonadError
  def ensure(error: => Throwable)(predicate: A => Boolean): IO[A] =
    ensureOr(_ => error)(predicate)

  // Turns a successful value into an error specified by the `error` function
  // if it does not satisfy a given predicate. See cats.MonadError
  def ensureOr(error: A => Throwable)(predicate: A => Boolean): IO[A] =
    flatMap(a => if (predicate(a)) pure(a) else raiseError(error(a)))

  def bracketCase[B](use: A => IO[B])(release: (A, ExitCase[Throwable]) => IO[Unit]): IO[B] =
    IO {
      this flatMap { resource =>
        try {
          import cats.syntax.apply._
          use(resource) <* release(resource, ExitCase.complete)
        } catch {
          case t: Throwable =>
            release(resource, ExitCase.error(t))
            throw t
        }
      }
    }.flatten
}

object IO {

  private case class Pure[A](a: A) extends IO[A] {
    override def unsafeRun(cb: Try[A] => Unit): Unit = cb(Success(a))
  }

  private case class Eval[A](thunk: () => A) extends IO[A] {
    override def unsafeRun(cb: Try[A] => Unit): Unit = cb(Success(thunk()))
  }

  private case class Error[A](exception: Throwable) extends IO[A] {
    override def unsafeRun(cb: Try[A] => Unit): Unit = cb(Failure(exception))
  }

  private case class Suspend[A](thunk: () => IO[A]) extends IO[A] {

    override def unsafeRun(cb: Try[A] => Unit): Unit = {
      val ioa: IO[A] = thunk()
      ioa.unsafeRun(cb)
    }
  }

  private case class FlatMap[A, B](ioa: IO[A], f: A => IO[B]) extends IO[B] {

    override def unsafeRun(cb: Try[B] => Unit): Unit =
      ioa.unsafeRun {
        case Success(a) => f(a).unsafeRun(cb)
        case Failure(t) => throw t // IO.raiseError(t)
      }
  }

  private case class FromFuture[A](fa: Future[A])(implicit ec: ExecutionContext) extends IO[A] {

    override def unsafeRun(cb: Try[A] => Unit): Unit =
      unsafeRunOnComplete(cb)
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

  def fromFuture[A](future: Future[A])(implicit ec: ExecutionContext): IO[A] = FromFuture(future)

  def deferFuture[A](future: => Future[A])(implicit ec: ExecutionContext): IO[A] =
    defer(IO.fromFuture(future))

  // Bracket instance defined in implicit scope
  implicit def ioMonad: Sync[IO] = new Sync[IO] {

    // Monad
    override def pure[A](value: A): IO[A]                              = IO.pure(value)
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B]        = fa flatMap f
    override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???

    // MonadError
    override def raiseError[A](e: Throwable): IO[A] = raiseError(e)

    override def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] =
      fa onErrorHandleWith f

    // Bracket
    override def bracketCase[A, B](
        acquire: IO[A]
    )(use: A => IO[B])(release: (A, ExitCase[Throwable]) => IO[Unit]): IO[B] =
      acquire.bracketCase(use)(release)

    // Sync
    override def suspend[A](thunk: => IO[A]): IO[A] = IO.suspend(thunk)
  }

  implicit class syntax[A](ioa: IO[A]) { // provide corresponding methods of ApplicativeError/MonadError

    def handleErrorWith(f: Throwable => IO[A]): IO[A]             = ioa onErrorHandleWith f
    def handleError(f: Throwable => A): IO[A]                     = ioa onErrorHandle f
    def recoverWith(pf: PartialFunction[Throwable, IO[A]]): IO[A] = ioa onErrorRecoverWith pf
    def recover(pf: PartialFunction[Throwable, A]): IO[A]         = ioa onErrorRecover pf

    def bracket[B](use: A => IO[B])(release: A => IO[Unit]): IO[B] =
      ioa.bracketCase(use)((a, _) => release(a))
  }
}
