Global / onChangedBuildSource := ReloadOnSourceChanges

val projectName = "wacccompiler"

lazy val root = (project in file("."))
    .settings(
        name := "wacccompiler",
        organization := "uk.ac.imperial.doc",
        scalaVersion := "2.13.7",
        version := "0.1.0",

        libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.2",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test,

        scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
   )
