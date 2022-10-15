import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.17",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "Scala AIS codec",
    libraryDependencies ++= Seq(
      "org.scodec" %% "scodec-bits" % "1.1.12",
      "org.scodec" %% "scodec-core" % "1.11.4",
      scalaTest % Test
    ),
    ThisBuild / turbo := true
  )
