package iomonad.experimental

import cats.Monad
import iomonad.auth.User

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/*
  Step 21 provides IO.deferFutureAction which allows to provide the EC when the IO is run.
  - I expanded the ADT IO again with a new subtype FutureToTask which wraps a function of type: ExecutionContext => Future[A]
  - IO.deferFutureAction just creates an instance of FutureToTask passing the function it received to it.
  - The IO#run method in trait IO has a new case for FutureToTask which applies the wrapped function to the implicitly passed EC,
    turns it into an IO with fromFuture and runs it passing the ec again.
 */
object IOApp21DeferFutureAction extends App {

  sealed trait IO[+A] extends Product with Serializable {

    import IO._

    protected def run(implicit ec: ExecutionContext): A

    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def map[B](f: A => B): IO[B] = flatMap(a => pure(f(a)))
    def flatten[B](implicit ev: A <:< IO[B]): IO[B] = flatMap(a => a)

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
      override def run(implicit ec: ExecutionContext): A = thunk()
    }
    private case class Eval[A](thunk: () => A) extends IO[A] {
      override def run(implicit ec: ExecutionContext): A = thunk()
    }
    private case class Error[A](exception: Throwable) extends IO[A] {
      override def run(implicit ec: ExecutionContext): A = throw exception
    }
    private case class Failed[A](io: IO[A]) extends IO[Throwable] {
      override def run(implicit ec: ExecutionContext): Throwable = try {
        io.run
        throw new NoSuchElementException("failed")
      } catch {
        case nse: NoSuchElementException if nse.getMessage == "failed" => throw nse
        case throwable: Throwable => throwable
      }
    }
    private case class Suspend[A](thunk: () => IO[A]) extends IO[A] {
      override def run(implicit ec: ExecutionContext): A = thunk().run
    }
    private case class FlatMap[A, B](src: IO[A], f: A => IO[B]) extends IO[B] {
      override def run(implicit ec: ExecutionContext): B = f(src.run).run
    }
    private case class FromFuture[A](fa: Future[A]) extends IO[A] {
      override def run(implicit ec: ExecutionContext): A = Await.result(fa, Duration.Inf) // BLOCKING!!!
    }
    private case class FutureToTask[A](ec2Future: ExecutionContext => Future[A]) extends IO[A] {
      override def run(implicit ec: ExecutionContext): A = fromFuture(ec2Future(ec)).run(ec)
    }

    def pure[A](a: A): IO[A] = Pure { () => a }
    def now[A](a: A): IO[A] = pure(a)

    def raiseError[A](exception: Exception): IO[A] = Error[A](exception)

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

    def deferFutureAction[A](ec2Future: ExecutionContext => Future[A]): IO[A] =
      FutureToTask(ec2Future)

    // Monad instance defined in implicit scope
    implicit val ioMonad: Monad[IO] = new Monad[IO] {
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

    println("\n>>> IO.deferFuture(future)")
    println("----- side effect performed lazily")
    val io = IO.deferFuture { futureGetUsers }

    io foreach { users => users foreach println } // prints "side effect"
    io foreach { users => users foreach println } // prints "side effect"
    Thread sleep 1000L
  }

  {
    println("\n>>> IO.deferFutureAction(implicit ec => future)")
    println("----- side effect performed lazily")
    val io = IO.deferFutureAction { implicit ec: ExecutionContext => futureGetUsers }

    // EC needed to run the IO
    implicit val ec: ExecutionContext = ExecutionContext.global

    io foreach { users => users foreach println } // prints "side effect"
    io foreach { users => users foreach println } // prints "side effect"
    Thread sleep 1000L
  }

  println("-----\n")
}
