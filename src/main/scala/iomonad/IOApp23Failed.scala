package iomonad

import java.util.NoSuchElementException

import cats.Monad

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/*
  Step 23
 */
object IOApp23Failed extends App {

  sealed trait IO[A] {

    import IO._

    private def run(implicit ec: ExecutionContext): A = {
      //println(s">>>>>>>>>>>> run: $this")
      this match {
        case Pure(thunk) => thunk()
        case Eval(thunk) => thunk()
        case Suspend(thunk) => thunk().run
        case FlatMap(src, f) => f(src.run).run
        case FutureToTask(ec2Future) => fromFuture(ec2Future(ec)).run(ec)
        case Error(exception) => throw exception
        case failed: Failed[A] => failed.get(ec).asInstanceOf[A]
      }
    }

    def map[B](f: A => B): IO[B] = flatMap(a => pure(f(a)))
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

    // ----- impure sync run* methods

    // runs on the current Thread returning Try[A]
    def runToTry(implicit ec: ExecutionContext): Try[A] = Try { run }

    // runs on the current Thread returning Either[Throwable, A]
    def runToEither(implicit ec: ExecutionContext): Either[Throwable, A] = runToTry.toEither

    // ----- impure async run* methods

    // returns a Future that runs the task eagerly on another thread
    def runToFuture(implicit ec: ExecutionContext): Future[A] = Future { run }

    // runs the IO in a Runnable on the given ExecutionContext
    // and then executes the specified Try based callback
    def runOnComplete(callback: Try[A] => Unit)(implicit ec: ExecutionContext): Unit =
      runAsync(ea => callback(ea.toTry)) // convert Try based callback into an Either based callback

    // runs the IO in a Runnable on the given ExecutionContext
    // and then executes the specified Either based callback
    def runAsync(callback: Either[Throwable, A] => Unit)(implicit ec: ExecutionContext): Unit =
      runAsync0(ec, callback)

    private def runAsync0(ec: ExecutionContext, callback: Either[Throwable, A] => Unit): Unit =
      ec.execute(() => callback(runToEither(ec)))

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

    private case class Pure[A](thunk: () => A) extends IO[A]
    private case class Eval[A](thunk: () => A) extends IO[A]
    private case class Suspend[A](thunk: () => IO[A]) extends IO[A]
    private case class FlatMap[A, B](src: IO[A], f: A => IO[B]) extends IO[B]
    private case class FutureToTask[A](f: ExecutionContext => Future[A]) extends IO[A]
    private case class Error[A](exception: Throwable) extends IO[A]
    private case class Failed[A](io: IO[A]) extends IO[Throwable] {
      def get(implicit ec: ExecutionContext): Throwable =
        try {
          io.run
          throw new NoSuchElementException("failed")
        } catch {
          case nse: NoSuchElementException if nse.getMessage == "failed" => throw nse
          case t: Throwable => t
        }
    }

    def pure[A](a: A): IO[A] = Pure { () => a }
    def now[A](a: A): IO[A] = pure(a)

    def raiseError[A](exception: Exception): IO[A] = Error[A](exception)
    def failed[A](exception: Exception): IO[A] = raiseError(exception) // analogous to Future.failed

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

    def fromFuture[A](fa: Future[A]): IO[A] =
      fa.value match {
        case Some(try0) => fromTry(try0)
        case None => IO.eval { Await.result(fa, Duration.Inf) } // BLOCKING!!!
      }

    def deferFuture[A](fa: => Future[A]): IO[A] =
      defer(IO.fromFuture(fa))

    def deferFutureAction[A](ec2Future: ExecutionContext => Future[A]): IO[A] =
      FutureToTask(ec2Future)

    implicit def ioMonad: Monad[IO] = new Monad[IO] {
      override def pure[A](value: A): IO[A] = IO.pure(value)
      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
      override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???
    }
  }



  println("\n-----")

  {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val ioError: IO[Int] = IO.raiseError[Int](new IllegalStateException("illegal state"))
    println(ioError.runToEither)
    //=> Left(java.lang.IllegalStateException: illegal state)

    println(">> projected:")
    val failed: IO[Throwable] = ioError.failed
    println(failed.runToEither)
    //=> Right(java.lang.IllegalStateException: illegal state)

    println

    val ioSuccess = IO.pure(5)
    println(ioSuccess.runToEither)
    //=> 5

    println(">> projected:")
    println(ioSuccess.failed.runToEither)
    //=> Left(java.util.NoSuchElementException: failed)
  }

  Thread sleep 500L
  println("-----\n")
}
