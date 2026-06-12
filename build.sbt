ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.3"

lazy val root = (project in file("."))
  .settings(
    name := "scala-dev-mooc-2026-04"
  )

libraryDependencies ++= Seq(
//  "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
  "org.scalactic" %% "scalactic" % "3.2.20",
  "org.scalactic" %% "scalactic" % "3.2.20" % Test,
  "org.scalatest" %% "scalatest" % "3.2.20" % Test
)

//resolvers += "Maven Central" at "https://repo1.maven.org/maven2/"