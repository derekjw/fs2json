name := "fs2json-jawn"

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

Common.settings