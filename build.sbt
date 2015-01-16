name := "scalac-chess-problem"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

mainClass in (Compile,run) := Some("ChessProblem")

scalacOptions ++= Seq("-optimise")

fork in run := true
javaOptions in run  ++= Seq( "-Xms4g", "-Xmx4g", "-XX:+UseParallelGC", "-XX:+UseParallelOldGC", "-XX:MaxNewSize=3g")
