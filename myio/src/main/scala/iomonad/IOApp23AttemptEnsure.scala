package iomonad

import cats.MonadError

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Random, Try}

/*
  Step 23 adds attempt, ensure and ensureOr.
 */
object IOApp23AttemptEnsure extends util.App {

  sealed trait IO[+A] extends Product with Serializable {

    import IO._

    protected def run(): A

    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

    def map[B](f: A => B): IO[B] = flatMap(a => pure(f(a)))

    def flatten[B](implicit ev: A <:< IO[B]): IO[B] = flatMap(a => a)

    // ----- impure sync run* methods

    // runs on the current Thread returning Try[A]
    def runToTry: Try[A] = Try {
      run()
    }

    // runs on the current Thread returning Either[Throwable, A]
    def runToEither: Either[Throwable, A] = runToTry.toEither

    // ----- impure async run* methods

    // returns a Future that runs the task eagerly on another thread
    def runToFuture(implicit ec: ExecutionContext): Future[A] = Future {
      run()
    }

    // runs the IO in a Runnable on the given ExecutionContext
    // and then executes the specified Try based callback
    def runOnComplete(callback: Try[A] => Unit)(implicit ec: ExecutionContext): Unit =
      runToFuture onComplete callback

    // runs the IO in a Runnable on the given ExecutionContext
    // and then executes the specified Either based callback
    def runAsync(callback: Either[Throwable, A] => Unit)(implicit ec: ExecutionContext): Unit =
      runOnComplete(tryy => callback(tryy.toEither))

    // Triggers async evaluation of this IO, executing the given function for the generated result.
    // WARNING: Will not be called if this IO is never completed or if it is completed with a failure.
    // Since this method executes asynchronously and does not produce a return value,
    // any non-fatal exceptions thrown will be reported to the ExecutionContext.
    def foreach(f: A => Unit)(implicit ec: ExecutionContext): Unit =
      runAsync {
        case Left(ex)     => ec.reportFailure(ex)
        case Right(value) => f(value)
      }

    // Returns a failed projection of this task.
    //
    // The failed projection is a Task holding a value of type Throwable, emitting the error yielded by the source,
    // in case the source fails, otherwise if the source succeeds the result will fail with a NoSuchElementException.
    def failed: IO[Throwable] = Failed(this)

    def onErrorHandleWith[AA >: A](f: Throwable => IO[AA]): IO[AA] =
      IO {
        this.runToEither match {
          case Left(t)  => f(t)
          case Right(a) => IO.pure(a)
        }
      }.flatten

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

    // Turns a successful value into an error specified by the `error` function if it does not satisfy a given predicate. See cats.MonadError
    def ensureOr(error: A => Throwable)(predicate: A => Boolean): IO[A] =
      IO {
        this.runToEither match {
          case Left(throwable)                  => raiseError(throwable)
          case Right(value) if predicate(value) => pure(value)
          case Right(value)                     => raiseError(error(value))
        }
      }.flatten
  }

  object IO {

    private case class Pure[A](thunk: () => A) extends IO[A] {
      override def run(): A = thunk()
    }

    private case class Eval[A](thunk: () => A) extends IO[A] {
      override def run(): A = thunk()
    }

    private case class Error[A](exception: Throwable) extends IO[A] {
      override def run(): A = throw exception
    }

    private case class Failed[A](io: IO[A]) extends IO[Throwable] {

      override def run(): Throwable =
        try {
          io.run()
          throw new NoSuchElementException("failed")
        } catch {
          case nse: NoSuchElementException if nse.getMessage == "failed" => throw nse
          case throwable: Throwable                                      => throwable
        }
    }

    private case class Suspend[A](thunk: () => IO[A]) extends IO[A] {
      override def run(): A = thunk().run()
    }

    private case class FlatMap[A, B](src: IO[A], f: A => IO[B]) extends IO[B] {
      override def run(): B = f(src.run()).run()
    }

    private case class FromFuture[A](fa: Future[A]) extends IO[A] {
      override def run(): A = Await.result(fa, Duration.Inf) // BLOCKING!!!
      // A solution of this problem would require a redesign of this simple IO Monod, which doesn't really support async computations.
    }

    def pure[A](a: A): IO[A] = Pure { () =>
      a
    }
    def now[A](a: A): IO[A] = pure(a)

    def raiseError[A](t: Throwable): IO[A] = Error[A](t)

    def eval[A](a: => A): IO[A] = Eval { () =>
      a
    }
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

      def handleErrorWith(f: Throwable => IO[A]): IO[A]             = ioa onErrorHandleWith f
      def handleError(f: Throwable => A): IO[A]                     = ioa onErrorHandle f
      def recoverWith(pf: PartialFunction[Throwable, IO[A]]): IO[A] = ioa onErrorRecoverWith pf
      def recover(pf: PartialFunction[Throwable, A]): IO[A]         = ioa onErrorRecover pf
    }
  }

  val r = Random

  def even(num: Int): Boolean = num % 2 == 0
  def odd(num: Int): Boolean  = !even(num)

  val io = IO {
    val num = r.nextInt(100)
    if (even(num))
      num
    else
      throw new IllegalStateException("odd number")
  }

  println("\n----- attempt:")

  val outer: Either[Throwable, Either[Throwable, Int]] = io.attempt.runToEither
  println(outer)
  val inner = outer.flatMap(x => x) // 2.12 or ... in 2.13 .flatten
  println(inner)

  Thread sleep 500L
  println("\n----- ensure:")

  println(io.ensure(new IllegalStateException("not divisable by 10"))(_ % 10 == 0).runToEither)

  Thread sleep 500L
  println("\n----- ensureOr:")

  println(
    io.ensureOr(num => new IllegalStateException(s"$num not divisable by 10"))(_ % 10 == 0)
      .runToEither
  )

  Thread sleep 500L
}
