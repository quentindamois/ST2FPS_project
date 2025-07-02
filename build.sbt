ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "edu.efrei"

lazy val root = ( project in file (".") )
  .settings (
  name := "library-management-system",
libraryDependencies ++= Seq (
  "org.scalatest" %% "scalatest" % "3.2.17" % Test ,
  "org.scalacheck" %% "scalacheck" % "1.17.0" % Test ,
 "org.typelevel" %% "cats-core" % "2.10.0",
 "io.circe" %% "circe-core" % "0.14.6",
 "io.circe" %% "circe-generic" % "0.14.6",
 "io.circe" %% "circe-parser" % "0.14.6"
)
)
