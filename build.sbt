name := "implementing-io-monad"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.0-M5"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",     // source files are in UTF-8
  "-deprecation",           // warn about use of deprecated APIs
  "-unchecked",             // warn about unchecked type parameters
  "-feature",               // warn about misused language features
  //"-Ypartial-unification",  // allow the compiler to unify type constructors of different arities
  //"-Xlint",                 // enable handy linter warnings
  //"-Xfatal-warnings",        // turn compiler warnings into errors
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.5.0" withSources() withJavadoc(),
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
