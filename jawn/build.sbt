organization := "com.github.derekjw"

name := "fs2json-jawn"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.spire-math" %% "jawn-parser" % "0.11.1",
  "com.lihaoyi" %% "utest" % "0.5.3" % Test,
)

libraryDependencies ++= Seq(
  "circe-core",
  "circe-generic",
  "circe-parser",
  "circe-testing",
  "circe-literal"
).map("io.circe" %% _ % "0.9.1" % Test)

testFrameworks += new TestFramework("utest.runner.Framework")

Release.settings