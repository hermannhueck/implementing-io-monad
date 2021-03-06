package myio.io24Bracket

import scala.concurrent.ExecutionContext

object AppBracket extends util.App {

  println(">>> bracket:")

  implicit val ec: ExecutionContext = ExecutionContext.global

  val acquire: IO[Unit] = IO {
    println("Resource acquired")
  }

  acquire.bracket { _ =>
    IO { println("Resource used") }
  } { _ =>
    IO { println(s"Resource released") }
  } foreach { _ =>
    ()
  }

  Thread sleep 500L
}
