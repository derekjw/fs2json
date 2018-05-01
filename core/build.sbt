name := "fs2json-core"

libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "0.10.3",
  "com.lihaoyi" %% "utest" % "0.5.3" % Test
)

libraryDependencies ++= Seq(
  "circe-core",
  "circe-generic",
  "circe-parser",
  "circe-testing"
).map("io.circe" %% _ % "0.9.1" % Test)

Common.settings
