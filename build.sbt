Global / onChangedBuildSource := ReloadOnSourceChanges

val projectName = "wacccompiler"

lazy val sbtAssemblySettings = baseAssemblySettings ++ Seq(
  assembly / assemblyOutputPath := baseDirectory.value / s"$projectName",
  assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case _                             => MergeStrategy.first
  }
)

lazy val root = (project in file("."))
    .settings(
      name := "wacccompiler",
      organization := "uk.ac.imperial.doc",
      scalaVersion := "2.13.7",
      version := "0.1.0",

      // SBT Assembly Settings
      sbtAssemblySettings,
      libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.5",
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test,
      scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")
    )
