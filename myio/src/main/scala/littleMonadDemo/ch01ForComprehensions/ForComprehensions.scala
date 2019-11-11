package littleMonadDemo.ch01ForComprehensions
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

object ForComprehensions extends util.App {

  def compute(list1: List[Int], list2: List[Int]): List[(Int, Int)] =
    for {
      i1 <- list1
      i2 <- list2
    } yield (i1, i2)

  def compute2(list1: List[Int], list2: List[Int]) =
    list1.flatMap { i1 =>
      list2.map { i2 =>
        (i1, i2)
      }
    }

  val l1 = List(1, 2, 3)
  val l2 = List(1, 20, 30, 40)

  val lResult = compute(l1, l2)
  println(lResult)

  val lResult2 = compute2(l1, l2)
  println(lResult2)

  def compute(option1: Option[Int], option2: Option[Int]): Option[(Int, Int)] =
    for {
      i1 <- option1
      i2 <- option2
    } yield (i1, i2)

  val o1 = Some(1)
  val o2 = Some(10)

  val oResult = compute(o1, o2)
  println(oResult)

  def compute(future1: Future[Int], future2: Future[Int]): Future[(Int, Int)] =
    for {
      i1 <- future1
      i2 <- future2
    } yield (i1, i2)

  val f1 = Future(1)
  val f2 = Future(10)

  val fResult = compute(f1, f2)
  val res     = Await.result(fResult, 3.seconds)
  println(res)
}
