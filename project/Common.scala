import com.lucidchart.sbt.scalafmt.ScalafmtCorePlugin.autoImport.scalafmtOnCompile
import sbt._
import sbt.Keys._

object Common {
  val scalaSettings = Seq(
    scalaVersion := "2.12.7",
    scalacOptions += "-Ypartial-unification",
    scalafmtOnCompile := true
  )

  val testSettings = Seq(
    testFrameworks := Seq(new TestFramework("utest.runner.Framework")),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "utest" % "0.6.5" % Test
    ) ++ Seq(
      "circe-core",
      "circe-generic",
      "circe-parser",
      "circe-testing",
      "circe-literal"
    ).map("io.circe" %% _ % "0.10.0" % Test)
  )

  val releaseSettings = Seq(
    organization := "com.github.derekjw",

    pomIncludeRepository := { _ => false },

    licenses := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.php")),

    homepage := Some(url("https://github.com/derekjw/fs2json")),

    scmInfo := Some(
      ScmInfo(
        url("https://github.com/derekjw/fs2json"),
        "scm:https://github.com/derekjw/fs2json.git"
      )
    ),

    developers := List(
      Developer(
        id = "derekjw",
        name = "Derek Williams",
        email = "derek@nebvin.ca",
        url = url("https://github.com/derekjw")
      )
    ),

    publishMavenStyle := true,

    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },

    publishArtifact in Test := false
  )

  val settings: Seq[Def.Setting[_]] = scalaSettings ++ testSettings ++ releaseSettings
}
