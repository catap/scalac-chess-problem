name := "scalac-chess-problem"

sbtPlugin := true

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

mainClass in (Compile,run) := Some("ChessProblem")