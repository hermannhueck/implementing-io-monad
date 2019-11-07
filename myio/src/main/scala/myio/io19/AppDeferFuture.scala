package myio.io19

import myio.auth.User

import scala.concurrent.{ExecutionContext, Future}

object AppDeferFuture extends util.App {

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
