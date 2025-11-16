ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "proyecto_final_pf",

    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "com.storm-enroute" %% "scalameter" % "0.19"
    ),

    Test / fork := true,
    Test / javaOptions += "-server"
  )
