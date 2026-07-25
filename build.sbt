ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

lazy val root = (project in file("."))
  .settings(
    name := "claude-demo",
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.5.16",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
    )
  )
