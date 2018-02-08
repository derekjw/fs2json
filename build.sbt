name := "fs2json"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "0.10.0",
  "com.lihaoyi" %% "utest" % "0.5.3" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")