package iomonad

import iomonad.auth._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/*
  Step 12 implements 'IO.raiseError'.
  It adds sub type Error to the ADT IO.
 */
object IOApp12RaiseError extends App {

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
        case Left(ex) => ec.reportFailure(ex)
        case Right(value) => f(value)
      }
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

    def pure[A](a: A): IO[A] = Pure { () => a }
    def now[A](a: A): IO[A] = pure(a)

    def raiseError[A](t: Throwable): IO[A] = Error[A](t)

    def eval[A](a: => A): IO[A] = Eval { () => a }
    def delay[A](a: => A): IO[A] = eval(a)
    def apply[A](a: => A): IO[A] = eval(a)
  }



  import User._
  import Password._

  def authenticate(username: String, password: String): IO[Boolean] =
    for {
      optUser <- IO(getUsers) map { users =>
        users.find(_.name == username)
      }
      isAuthenticated <- IO(getPasswords) map { passwords =>
        optUser.isDefined && passwords.contains(Password(optUser.get.id, password))
      }
    } yield isAuthenticated



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
  val tryy: Try[Boolean] = checkMaggie.runToTry
  println(tryy)
  //=> Success(true)

  println("\n>>> IO#runToEither:")
  val either: Either[Throwable, Boolean] = checkMaggie.runToEither
  println(either)
  //=> Right(true)

  println("\n>>> IO#runToFuture:")
  checkMaggie.runToFuture onComplete authCallbackTry
  Thread sleep 500L

  println("\n>>> IO#runOnComplete:")
  checkMaggie runOnComplete authCallbackTry
  Thread sleep 500L

  println("\n>>> IO#runAsync:")
  checkMaggie runAsync authCallbackEither
  Thread sleep 500L


  println("\n-----")

  println(">> source: ioError")
  val ioError: IO[Int] = IO.raiseError[Int](new IllegalStateException("illegal state"))
  println(ioError.runToEither)
  //=> Left(java.lang.IllegalStateException: illegal state)

  Thread sleep 500L
  println("-----\n")
}
