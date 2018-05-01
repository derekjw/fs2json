import sbtrelease.ReleaseStateTransformations._

name := "fs2json"

lazy val fs2json = project in file(".") aggregate (core, jawn, circe)

lazy val core = project in file("core")

lazy val jawn = project in file("jawn") dependsOn core

lazy val circe = project in file("circe") dependsOn jawn

Common.settings

publishArtifact := false

releaseCrossBuild := true // true if you cross-build the project for multiple Scala versions

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommand("publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)
