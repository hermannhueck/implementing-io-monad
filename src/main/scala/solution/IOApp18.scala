package solution

import cats.Monad
import solution.auth._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/*
  In step 18 the synchronous run* methods (run, runToTry, runToEither)
  all take an implicit EC in order to enable async execution as well.
  That's what we need in the next step.
 */
object IOApp18 extends App {

  trait IO[A] {

    import IO._

    private def run(implicit ec: ExecutionContext): A = this match {
      case Pure(thunk) => thunk()
      case Eval(thunk) => thunk()
      case Suspend(thunk) => thunk().run
      case FlatMap(src, f) => f(src.run).run
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
      // convert Try based callback into an Either based callback
      runAsync0(ec, (ea: Either[Throwable, A]) => callback(ea.toTry))

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
  }

  object IO {

    private case class Pure[A](thunk: () => A) extends IO[A]
    private case class Eval[A](thunk: () => A) extends IO[A]
    private case class Suspend[A](thunk: () => IO[A]) extends IO[A]
    private case class FlatMap[A, B](src: IO[A], f: A => IO[B]) extends IO[B]

    def pure[A](a: A): IO[A] = Pure { () => a }
    def now[A](a: A): IO[A] = pure(a)

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
        case None => IO.eval { Await.result(fa, Duration.Inf) } // eval is lazy!
      }

    def deferFuture[A](fa: => Future[A]): IO[A] =
      defer(IO.fromFuture(fa))

    implicit def ioMonad: Monad[IO] = new Monad[IO] {
      override def pure[A](value: A): IO[A] = IO.pure(value)
      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
      override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???
    }
  }



  def futureGetUsers(implicit ec: ExecutionContext): Future[Seq[User]] = {
    Future {
      println("===> side effect <===")
      User.getUsers
    }
  }

  {
    // EC needed to turn a Future into an IO
    implicit val ec: ExecutionContext = ExecutionContext.global

    println("\n>>> IO.defer(IO.fromFuture(future))")
    println("----- side effect performed lazily")
    val io = IO.defer { IO.fromFuture { futureGetUsers } }

    io foreach { users => users foreach println } // prints "side effect"
    io foreach { users => users foreach println } // prints "side effect"
    Thread sleep 1000L
  }

  {
    // EC needed to turn a Future into an IO
    implicit val ec: ExecutionContext = ExecutionContext.global

    println("\n>>> IO.deferFuture(future)")
    println("----- side effect performed lazily")
    val io = IO.deferFuture { futureGetUsers }

    io foreach { users => users foreach println } // prints "side effect"
    io foreach { users => users foreach println } // prints "side effect"
    Thread sleep 1000L
  }

  println("-----\n")
}
