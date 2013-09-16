import sbt._
import Keys._

object UnionFindBuild extends Build {

  lazy val project = Project(
    id = "unionfind",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "unionfind",
      scalaVersion := "2.10.2",
      version := "0.1-SNAPSHOT",
      libraryDependencies ++= Seq(
        "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
      )
    )
  )

}
