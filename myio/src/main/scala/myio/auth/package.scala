package myio

package object auth {

  import scala.util.Try

  def printAuthTry[A](tryy: Try[A]): Unit =
    println(
      tryy.fold(
        ex => ex.toString,
        isAuthenticated => s"isAuthenticated = $isAuthenticated"
      )
    )

  def printAuthEither[A](either: Either[Throwable, A]): Unit =
    println(
      either.fold(
        ex => ex.toString,
        isAuthenticated => s"isAuthenticated = $isAuthenticated"
      )
    )

  def authCallbackTry[A]: Try[A] => Unit = printAuthTry

  def authCallbackEither[A]: Either[Throwable, A] => Unit = printAuthEither

  import scala.util.Using
  import scala.io.Source
  import java.net.URL

  def linesFromUrl(url: URL): List[String] =
    Using.resource {
      Source.fromURL(url)
    } {
      _.getLines.toList
    }
}
