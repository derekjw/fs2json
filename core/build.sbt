organization := "com.github.derekjw"

name := "fs2json-core"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "0.10.2",
  "com.lihaoyi" %% "utest" % "0.5.3" % Test,
)

libraryDependencies ++= Seq(
  "circe-core",
  "circe-generic",
  "circe-parser",
  "circe-testing"
).map("io.circe" %% _ % "0.9.1" % Test)

testFrameworks += new TestFramework("utest.runner.Framework")

Release.settings