package myio.io26WithAllApps

import myio.auth.User

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object App19GetUsers extends util.App {

  println("\n>>> IO#fromTry:")
  val tryy: Try[Seq[User]] = Try { User.getUsers }
  val io1: IO[Seq[User]]   = IO.fromTry(tryy)
  io1.unsafeRunToEither foreach println

  println("\n>>> IO#fromEither:")
  val either: Either[Throwable, Seq[User]] = tryy.toEither
  val io2: IO[Seq[User]]                   = IO.fromEither(either)
  io1.unsafeRunToEither foreach println

  def futureGetUsers(implicit ec: ExecutionContext): Future[Seq[User]] = {
    Future {
      println("===> side effect <===")
      User.getUsers
    }
  }

  {
    // EC needed to turn a Future into an IO
    implicit val ec: ExecutionContext = ExecutionContext.global

    println("\n>>> IO.fromFuture(future)")
    println("----- side effect performed eagerly")

    val io = IO.fromFuture { futureGetUsers }
    io foreach { users =>
      users foreach println
    } // prints "side effect"
    io foreach { users =>
      users foreach println
    }
    Thread sleep 1000L
  }

  {
    // EC needed to turn a Future into an IO
    implicit val ec: ExecutionContext = ExecutionContext.global

    println("\n>>> IO.defer(IO.fromFuture(future))")
    println("----- side effect performed lazily")
    val io = IO.defer { IO.fromFuture { futureGetUsers } }

    io foreach { users =>
      users foreach println
    } // prints "side effect"
    io foreach { users =>
      users foreach println
    } // prints "side effect"
    Thread sleep 1000L
  }

  {
    // EC needed to turn a Future into an IO
    implicit val ec: ExecutionContext = ExecutionContext.global

    println("\n>>> IO.deferFuture(future)")
    println("----- side effect performed lazily")
    val io = IO.deferFuture { futureGetUsers }

    io foreach { users =>
      users foreach println
    } // prints "side effect"
    io foreach { users =>
      users foreach println
    } // prints "side effect"
    Thread sleep 1000L
  }
}
