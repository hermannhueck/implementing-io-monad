package iomonad

import cats.Monad
import iomonad.auth._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/*
  Step 17 provides IO.fromTry and IO.fromEither.
  These methods (eagerly) converts a Try or an Either into an IO.
 */
object IOApp17FromTryAndFromEither extends App {

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

    def pure[A](a: A): IO[A] = Pure { () => a }
    def now[A](a: A): IO[A] = pure(a)

    def raiseError[A](t: Throwable): IO[A] = Error[A](t)

    def eval[A](a: => A): IO[A] = Eval { () => a }
    def delay[A](a: => A): IO[A] = eval(a)
    def apply[A](a: => A): IO[A] = eval(a)

    def suspend[A](ioa: => IO[A]): IO[A] = Suspend(() => ioa)
    def defer[A](ioa: => IO[A]): IO[A] = suspend(ioa)

    def fromTry[A](tryy: Try[A]): IO[A] =
      tryy.fold(IO.raiseError, IO.pure)

    def fromEither[A](either: Either[Throwable, A]): IO[A] =
      either.fold(IO.raiseError, IO.pure)

    // Monad instance defined in implicit scope
    implicit val ioMonad: Monad[IO] = new Monad[IO] {
      override def pure[A](value: A): IO[A] = IO.pure(value)
      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
      override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???
    }
  }



  println("\n-----")

  implicit val ec: ExecutionContext = ExecutionContext.global

  val tryy: Try[Seq[User]] = Try { User.getUsers }
  val io1: IO[Seq[User]] = IO.fromTry(tryy)
  io1.runToEither foreach println

  println("-----")
  val either: Either[Throwable, Seq[User]] = tryy.toEither
  val io2: IO[Seq[User]] = IO.fromEither(either)
  io1.runToEither foreach println

  println("-----\n")
}
