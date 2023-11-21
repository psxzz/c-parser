ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "c-parser"
  )
