package util

package object formatting {

  import build._
  import Console._

  val sbtVersion       = BuildInfo.sbtVersion
  val scalaVersion     = BuildInfo.scalaVersion
  val buildInfo        = s"BuildInfo: sbt.version = $sbtVersion, scala.version = $scalaVersion"
  val buildInfoColored = s"${BLUE}$buildInfo${RESET}"
  val buildInfoLong    = BuildInfo.toString

  def javaRuntimeInfo = {
    val javaVendor  = System.getProperty("java.vendor")
    val javaVersion = System.getProperty("java.version")
    s"Java Runtime: $javaVendor, $javaVersion"
  }
  def javaRuntimeInfoColored = s"${BLUE}$javaRuntimeInfo${RESET}"

  def printHeader(
      text: String,
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): Unit =
    println(header(text, width, leading, trailing, fill))

  def header(
      text: String,
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): String = {
    val textColored = s"${BLUE}$text${RESET}"
    s"""|${textInLine(textColored, width, leading, trailing, fill)}
        |${textInLine(s"$javaRuntimeInfoColored", width, leading, trailing, fill)}
        |${textInLine(s"$buildInfoColored", width, leading, trailing, fill)}""".stripMargin
  }

  def printTextInLine(
      text: String,
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): Unit =
    println(textInLine(s"${BLUE}$text${RESET}", width, leading, trailing, fill))

  def textInLine(
      text: String,
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): String = {
    val frontPad    = fill * 10
    val startLength = (10 + text.length() + 2)
    val endLength   = if (startLength > width) 0 else width - startLength
    val endPad      = fill * (endLength + 9) // add 9 to adjust color escape chars
    s"$leading$frontPad $text $endPad$trailing"
  }

  def printLine(
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): Unit =
    println(line(width, leading, trailing, fill))

  def line(
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): String = {
    val line = fill * width
    s"$leading$line$trailing"
  }

  def dash(
      width: Int,
      fill: String = "\u2500"
  ): String =
    line(width, "", "", fill)

  def objectNameSimple(scalaObject: java.lang.Object) = {
    val cn = scalaObject.getClass().getSimpleName()
    cn.substring(0, cn.length() - 1)
  }

  def objectName(scalaObject: java.lang.Object) = {
    val cn = scalaObject.getClass().getName
    cn.substring(0, cn.length() - 1)
  }

  def printHeaderWithProgramName(scalaObject: java.lang.Object) =
    printHeader(objectName(scalaObject))

  def printProgramNameInLine(scalaObject: java.lang.Object) =
    printTextInLine(objectName(scalaObject))

  def printGreen(): Unit   = print(GREEN)
  def printRed(): Unit     = print(RED)
  def printBlue(): Unit    = print(BLUE)
  def printYellow(): Unit  = print(YELLOW)
  def printCyan(): Unit    = print(CYAN)
  def printMagenta(): Unit = print(MAGENTA)
  def printReset(): Unit   = print(RESET)
}
