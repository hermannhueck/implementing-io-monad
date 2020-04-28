package util

import java.lang.System.{currentTimeMillis => currentTime}
import scala.collection.mutable.ListBuffer
import scala.util.Using
import util.formatting._

// @com.github.ghik.silencer.silent("deprecated")
@scala.annotation.nowarn("cat=deprecation&since=2.11.0&msg=trait DelayedInit:ws")
trait App extends DelayedInit {

  final val executionStart: Long = currentTime

  final protected def args: Array[String] = _args

  private[this] var _args: Array[String] = _

  private[this] val initCode = new ListBuffer[() => Unit]

  def execBody(body: => Unit): Unit =
    Using.resource[Unit, Unit] {
      printHeaderWithProgramName(this)
    } { _ => body } { _ =>
      val total = currentTime - executionStart
      printFooter(s"[total: $total ms]")
    }

  override def delayedInit(body: => Unit): Unit = {
    initCode += (() => execBody(body))
  }

  final def main(args: Array[String]) = {
    this._args = args
    for (proc <- initCode) proc()
  }
}
