package iomonad

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/*
  Step 14 defines 3 methods which return IO[A]: sumIO, fibonacciIO, factorialIO
  Based on these methods it defines method 'computeIO' that uses these methods in a for-comprehension.
 */
object IOApp14PureComputations extends util.App {

  sealed trait IO[+A] extends Product with Serializable {

    import IO._

    protected def run(): A

    def flatMap[B](f: A => IO[B]): IO[B]            = FlatMap(this, f)
    def map[B](f: A => B): IO[B]                    = flatMap(a => pure(f(a)))
    def flatten[B](implicit ev: A <:< IO[B]): IO[B] = flatMap(a => a)

    // ----- impure sync run* methods

    // runs on the current Thread returning Try[A]
    def runToTry: Try[A] = Try { run() }

    // runs on the current Thread returning Either[Throwable, A]
    def runToEither: Either[Throwable, A] = runToTry.toEither

    // ----- impure async run* methods

    // returns a Future that runs the task eagerly on another thread
    def runToFuture(implicit ec: ExecutionContext): Future[A] = Future { run() }

    // takes a Try based callback
    def runOnComplete(callback: Try[A] => Unit)(implicit ec: ExecutionContext): Unit =
      runToFuture onComplete callback

    // takes a Either based callback
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
  }

  object IO {

    private case class Pure[A](thunk: () => A) extends IO[A] {
      override def run(): A = thunk()
    }

    private case class Eval[A](thunk: () => A) extends IO[A] {
      override def run(): A = thunk()
    }

    private case class FlatMap[A, B](src: IO[A], f: A => IO[B]) extends IO[B] {
      override def run(): B = f(src.run()).run()
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
  }

  def sumIO(from: Int, to: Int): IO[Int] =
    IO { sumOfRange(from, to) }

  def fibonacciIO(num: Int): IO[BigInt] =
    IO { fibonacci(num) }

  def factorialIO(num: Int): IO[BigInt] =
    IO { factorial(num) }

  def computeIO(from: Int, to: Int): IO[BigInt] =
    for {
      x <- sumIO(from, to)
      y <- fibonacciIO(x)
      z <- factorialIO(y.intValue)
    } yield z

  val io: IO[BigInt] = computeIO(1, 4)

  implicit val ec: ExecutionContext = ExecutionContext.global
  io foreach { result =>
    println(s"result = $result")
  }
  //=> 6227020800

  Thread sleep 500L
}
