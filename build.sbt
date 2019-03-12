name := "mojito"

version := "0.1"

scalaVersion := "2.12.8"

enablePlugins(MicrositesPlugin)

lazy val scala211      = "2.11.12"
lazy val scala212      = "2.12.8"

crossScalaVersions := List(scala212, scala211)