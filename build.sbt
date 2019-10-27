import sbt._
import Keys._
import xerial.sbt.Sonatype._

inThisBuild(
  Seq(
    organization := "com.github.mvv.routineer",
    version := "0.2-M2",
    homepage := Some(url("https://github.com/mvv/routineer")),
    scmInfo := Some(ScmInfo(url("https://github.com/mvv/routineer"), "scm:git@github.com:mvv/routineer.git")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(id = "mvv",
                name = "Mikhail Vorozhtsov",
                email = "mikhail.vorozhtsov@gmail.com",
                url = url("https://github.com/mvv"))
    ),
    sonatypeProjectHosting := Some(GitHubHosting("mvv", "routineer", "mikhail.vorozhtsov@gmail.com"))
  )
)

ThisBuild / publishTo := sonatypePublishTo.value
ThisBuild / publishMavenStyle := true

lazy val sonatypeOpenIfNotSnapshot: Command = Command.command("sonatypeOpenIfNotSnapshot") { state =>
  val extracted = Project.extract(state)
  if (extracted.get(isSnapshot)) {
    val log = extracted.get(sLog)
    log.info("Snapshot version, doing nothing")
    state
  } else {
    Command.process("sonatypeOpen", state)
  }
}

lazy val sonatypeReleaseIfNotSnapshot: Command = Command.command("sonatypeReleaseIfNotSnapshot") { state =>
  val extracted = Project.extract(state)
  if (extracted.get(isSnapshot)) {
    val log = extracted.get(sLog)
    log.info("Snapshot version, doing nothing")
    state
  } else {
    Command.process("sonatypeRelease", state)
  }
}

inThisBuild(
  Seq(
    crossScalaVersions := Seq("2.13.1", "2.12.10", "2.11.12"),
    scalaVersion := crossScalaVersions.value.head,
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xfatal-warnings")
  )
)

lazy val root = (project in file("."))
  .settings(
    crossScalaVersions := Nil,
    skip in publish := true,
    sonatypeProfileName := "com.github.mvv",
    sonatypeSessionName := s"Routineer_${version.value}",
    commands ++= Seq(sonatypeOpenIfNotSnapshot, sonatypeReleaseIfNotSnapshot)
  )
  .aggregate(core, cats, examples)

val specs2 = "org.specs2" %% "specs2-core" % "4.7.1" % Test

lazy val core = (project in file("core"))
  .settings(name := "routineer", libraryDependencies += specs2)

lazy val cats = (project in file("cats"))
  .settings(
    name := "routineer-cats",
    libraryDependencies ++=
      Seq("org.typelevel" %% "cats-core" % "2.0.0" % Provided, specs2)
  )
  .dependsOn(core)

lazy val examples = (project in file("examples"))
  .settings(
    name := "routineer-examples",
    crossScalaVersions := Nil,
    skip in publish := true
  )
  .aggregate(examplesServlet)

lazy val examplesServlet = (project in file("examples/servlet"))
  .enablePlugins(JettyPlugin)
  .settings(name := "routineer-examples-servlet",
            skip in publish := true,
            libraryDependencies +=
              "javax.servlet" % "javax.servlet-api" % "3.0.1" % Provided)
  .dependsOn(core)
