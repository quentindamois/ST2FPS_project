ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "edu.efrei"

lazy val root = (project in file("."))
  .settings(
    name := "library-management-system",
    libraryDependencies ++= Seq(
      // JSON handling
      "io.circe" %% "circe-core" % "0.14.6",
      "io.circe" %% "circe-generic" % "0.14.6",
      "io.circe" %% "circe-parser" % "0.14.6",
      "com.lihaoyi" %% "upickle" % "4.2.1",

      // Testing
      "org.scalactic" %% "scalactic" % "3.2.17",
      "org.scalatest" %% "scalatest" % "3.2.17" % "test",
      "org.scalacheck" %% "scalacheck" % "1.17.0" % "test",

      // Logging
      "ch.qos.logback" % "logback-classic" % "1.4.11",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",

      // Scala Test
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test
    )
  )
