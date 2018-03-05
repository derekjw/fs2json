organization := "com.github.derekjw"

name := "fs2json-circe"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.9.1",
  "io.circe" %% "circe-parser" % "0.9.1",
  "com.lihaoyi" %% "utest" % "0.5.3" % Test,
)

libraryDependencies ++= Seq(
  "circe-generic",
  "circe-testing",
  "circe-literal"
).map("io.circe" %% _ % "0.9.1" % Test)

testFrameworks += new TestFramework("utest.runner.Framework")

Release.settings