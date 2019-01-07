package iomonad

import cats.Monad
import iomonad.auth._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/*
  In step 16 I added the subtype FlatMap to the ADT IO and expanded the 'run' method accordingly.
  The Method IO#flatMap just creates an instance of FlatMap.
  IO#map is implemented in terms of IO#flatMap and IO.pure.
  Now IO is trampolined and hence stack-safe.
 */
object IOApp16ADTWithFlatMap extends App {

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

    // Monad instance defined in implicit scope
    implicit val ioMonad: Monad[IO] = new Monad[IO] {
      override def pure[A](value: A): IO[A] = IO.pure(value)
      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
      override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???
    }
  }



  import User._
  import Password._

  def authenticate(username: String, password: String): IO[Boolean] =
    for {
      optUser <- IO(getUsers) map { users =>
        users.find(_.name == username)
      }
      authenticated <- IO(getPasswords) map { passwords =>
        optUser.isDefined && passwords.contains(Password(optUser.get.id, password))
      }
    } yield authenticated



  println("\n-----")


  implicit val ec: ExecutionContext = ExecutionContext.global

  IO(getUsers) foreach { users => users foreach println }
  Thread sleep 500L
  println("-----")

  IO(getPasswords) foreach { users => users foreach println }
  Thread sleep 500L
  println("-----")

  println("\n>>> IO#foreach: authenticate:")
  authenticate("maggie", "maggie-pw") foreach println       //=> true
  Thread sleep 200L
  authenticate("maggieXXX", "maggie-pw") foreach println    //=> false
  Thread sleep 200L
  authenticate("maggie", "maggie-pwXXX") foreach println    //=> false
  Thread sleep 200L


  val checkMaggie: IO[Boolean] = authenticate("maggie", "maggie-pw")

  println("\n>>> IO#runToTry:")
  printAuthTry(checkMaggie.runToTry)

  println("\n>>> IO#runToEither:")
  printAuthEither(checkMaggie.runToEither)

  println("\n>>> IO#runToFuture:")
  checkMaggie.runToFuture onComplete authCallbackTry
  Thread sleep 500L

  println("\n>>> IO#runOnComplete:")
  checkMaggie runOnComplete authCallbackTry
  Thread sleep 500L

  println("\n>>> IO#runAsync:")
  checkMaggie runAsync authCallbackEither
  Thread sleep 500L

  println("-----")

  println("\n>>> IO.pure(...):")
  val io1 = IO.pure { println("immediate side effect"); 5 }
  //=> immediate side effect
  Thread sleep 2000L
  io1 foreach println
  //=> 5
  Thread sleep 2000L

  println("\n>>> IO.defer(IO.pure(...)):")
  val io2 = IO.defer { IO.pure { println("deferred side effect"); 5 } }
  Thread sleep 2000L
  io2 foreach println
  //=> deferred side effect
  //=> 5
  Thread sleep 2000L

  println("-----\n")
}
