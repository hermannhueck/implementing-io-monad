package iomonad

import cats.MonadError

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/*
  Step 19 provides IO.deferFuture which can make the Future lazy.
  IO.deferFuture(f) is just an alias for IO.defer { IO.fromFuture(f) }
 */
object IOApp20OnErrorHandleWith extends App {

  sealed trait IO[+A] extends Product with Serializable {

    import IO._

    protected def run(): A

    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def map[B](f: A => B): IO[B] = flatMap(a => pure(f(a)))
    def flatten[B](implicit ev: A <:< IO[B]): IO[B] = flatMap(a => a)

    // ----- impure sync run* methods

    // runs on the current Thread returning Try[A]
    def runToTry: Try[A] = Try { run() }

    // runs on the current Thread returning Either[Throwable, A]
    def runToEither: Either[Throwable, A] = runToTry.toEither

    // ----- impure async run* methods

    // returns a Future that runs the task eagerly on another thread
    def runToFuture(implicit ec: ExecutionContext): Future[A] = Future { run() }

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
        case Left(ex) => ec.reportFailure(ex)
        case Right(value) => f(value)
      }

    // Returns a failed projection of this task.
    //
    // The failed projection is a Task holding a value of type Throwable, emitting the error yielded by the source,
    // in case the source fails, otherwise if the source succeeds the result will fail with a NoSuchElementException.
    def failed: IO[Throwable] = Failed(this)

    def onErrorHandleWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = IO {
      this.runToEither match {
        case Left(t) => f(t)
        case Right(a) => IO.pure(a)
      }
    }.flatten

    def onErrorHandle[AA >: A](f: Throwable => AA): IO[AA] =
      onErrorHandleWith(t => IO.pure(f(t)))
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
      override def run(): Throwable = try {
        io.run()
        throw new NoSuchElementException("failed")
      } catch {
        case nse: NoSuchElementException if nse.getMessage == "failed" => throw nse
        case throwable: Throwable => throwable
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
    }

    def pure[A](a: A): IO[A] = Pure { () => a }
    def now[A](a: A): IO[A] = pure(a)

    def raiseError[A](t: Throwable): IO[A] = Error[A](t)

    def eval[A](a: => A): IO[A] = Eval { () => a }
    def delay[A](a: => A): IO[A] = eval(a)
    def apply[A](a: => A): IO[A] = eval(a)

    def suspend[A](ioa: => IO[A]): IO[A] = Suspend(() => ioa)
    def defer[A](ioa: => IO[A]): IO[A] = suspend(ioa)

    def fromTry[A](tryy: Try[A]): IO[A] = IO {
      tryy match {
        case Failure(t) => throw t
        case Success(value) => value
      }
    }

    def fromEither[A](either: Either[Throwable, A]): IO[A] = IO {
      either match {
        case Left(t) => throw t
        case Right(value) => value
      }
    }

    def fromFuture[A](future: Future[A]): IO[A] = FromFuture(future)

    def deferFuture[A](future: => Future[A]): IO[A] =
      defer(IO.fromFuture(future))

    // MonadError instance defined in implicit scope
    implicit def ioMonad: MonadError[IO, Throwable] = new MonadError[IO, Throwable] {

      // Monad
      override def pure[A](value: A): IO[A] = IO.pure(value)
      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
      override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???

      // MonadError
      override def raiseError[A](e: Throwable): IO[A] = raiseError(e)
      override def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] = fa onErrorHandleWith f
    }

    implicit class syntax[A](ioa: IO[A]) { // provide corresponding methods of ApplicativeError/MonadError

      def handleErrorWith(f: Throwable => IO[A]): IO[A] = ioa onErrorHandleWith f
      def handleError(f: Throwable => A): IO[A] = ioa onErrorHandle f
    }
  }


  println("\n----- onErrorHandleWith:")

  val error1: IO[Int] = IO.raiseError[Int](new IllegalStateException("illegal state"))
  val error2: IO[Int] = IO.raiseError[Int](new RuntimeException("dummy"))

  val completeHandler: Throwable => IO[Int] = {
    case ise: IllegalStateException =>
      IO.pure(-1)
    case t: Throwable =>
      IO.raiseError(t)
  }

  println(error1.onErrorHandleWith(completeHandler).runToEither)
  println(error2.onErrorHandleWith(completeHandler).runToEither)

  println("\n----- onErrorHandle:")

  val completeHandler2: Throwable => Int = {
    case ise: IllegalStateException => -1
    case t: Throwable => throw t
  }

  println(error1.onErrorHandle(completeHandler2).runToEither)
  println(error2.onErrorHandle(completeHandler2).runToEither)

  println("-----\n")
}
