import com.typesafe.sbt.packager.archetypes.JavaAppPackaging

val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
val colossus = "com.tumblr" % "colossus_2.11" % "0.8.3"
val parsing = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

lazy val root = (project in file(".")) 
  .settings(
    scalaVersion := "2.11.8",
    version      := "0.1.0-SNAPSHOT",
    name         := "Boggle",
    libraryDependencies ++= Seq(
      scalaTest,
      colossus,
      parsing
    )
  ).enablePlugins(JavaAppPackaging)

