import sbt._
import sbt.Keys._

object ProjectBuild extends Build {

  val projectName = "data-gen-scala"
  val projectVersion = "1"
  val projectOrg = "net.ximity"

  override def settings = super.settings ++ Seq(
    name := projectName,
    organization := projectOrg,
    version := projectVersion,
    scalaVersion in ThisBuild := "2.11.7",
    scalacOptions ++= Seq(
      "-Xlint",
      "-deprecation",
      "-Xfatal-warnings",
      "-feature",
      "-unchecked",
      "-encoding", "utf8")
  )

  import MainDependencies._
  import TestDependencies._
  import ScalariformSettings._

  val rootId = projectName + "-v" + projectVersion
  lazy val root = Project(id = rootId, base = file("."))
    .settings(
      customScalariformSettings,
      libraryDependencies ++= Seq(
        sjsonNew,
        scalatest
      )
    )
}

