import _root_.sbt.Keys._

name := "planook"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.8.0",
  "io.argonaut" %% "argonaut" % "6.0.4",
  "com.github.scopt" %% "scopt" % "3.3.0"
)