package myio.io26WithAllApps

import myio.effect.Sync
import scala.concurrent.ExecutionContext

object App26Sync extends util.App {

  implicit val ec: ExecutionContext = ExecutionContext.global

  println("\n>>> IO.pure(...):")
  val io1 = IO.pure { println("immediate side effect"); 5 }
  //=> immediate side effect
  Thread sleep 2000L
  io1 foreach println
  //=> 5
  Thread sleep 2000L

  println("\n>>> IO.suspend(IO.pure(...)):")
  val io2 = IO.suspend { IO.pure { println("suspended side effect"); 5 } }
  Thread sleep 2000L
  io2 foreach println
  //=> suspended side effect
  //=> 5
  Thread sleep 2000L

  println("\n>>> Sync[F].pure(...):")
  def sync1[F[_]: Sync]: F[Int] = Sync[F].pure { println("immediate side effect"); 5 }
  //=> immediate side effect
  Thread sleep 2000L
  sync1[IO] foreach println
  //=> 5
  Thread sleep 2000L

  println("\n>>> Sync[F].suspend(Sync[F].pure(...)):")

  def sync2[F[_]: Sync]: F[Int] = Sync[F].suspend {
    Sync[F].pure { println("suspended side effect"); 5 }
  }
  Thread sleep 2000L
  sync2[IO] foreach println
  //=> suspended side effect
  //=> 5
  Thread sleep 2000L

}
