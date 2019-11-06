import ScalacOptions._

name := "implementing-io-monad"
description := "own implementation of the IO Monad (supporting some of the cats-effect type classes)"
version := "0.0.1"

scalaVersion := "2.13.1"

scalacOptions ++= defaultScalacOptions

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "2.0.0"
)

// https://github.com/typelevel/kind-projector
addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)
// https://github.com/oleg-py/better-monadic-for
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
