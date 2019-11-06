import sbt._

object ScalacOptions {

  val defaultScalacOptions = Seq(
    "-encoding",
    "UTF-8",        // source files are in UTF-8
    "-deprecation", // warn about use of deprecated APIs
    "-unchecked",   // warn about unchecked type parameters
    "-feature",     // warn about misused language features
    "-Xlint",       // enable handy linter warnings
    // "-Xfatal-warnings", // fail the compilation if there are any warnings
    "-explaintypes", // explain type errors in more detail
    "-Xcheckinit"    // wrap field accessors to throw an exception on uninitialized access
  )
}
