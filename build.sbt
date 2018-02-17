organization := "com.github.derekjw"

name := "fs2json"

scalaVersion := "2.12.4"

lazy val fs2json = project in file(".") aggregate (core, jawn)

lazy val core = project in file("core")

lazy val jawn = project in file("jawn") dependsOn core

Release.settings