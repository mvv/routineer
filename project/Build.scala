import sbt._
import Keys._
import com.github.siasia.WebPlugin
import WebPlugin.{webSettings, webappResources}

object RoutineerBuild extends Build {
  val localMavenRepo =
    Resolver.file("local-maven", Path.userHome / ".m2" / "repository")(
                  Resolver.mavenStylePatterns)
  val publishLocalMavenConfiguration =
    TaskKey[PublishConfiguration](
      "publish-local-maven-configuration",
      "Configuration for publishing to the local maven repository.")
  val publishLocalMaven =
    TaskKey[Unit]("publish-local-maven",
                  "Publishes artifacts to the local maven repository.")

  val buildSettings = Seq(
    organization := "com.github.mvv.routineer",
    version := "0.1.2-SNAPSHOT",
    scalaVersion := "2.9.0-1",
    crossScalaVersions := Seq("2.8.1", "2.9.0-1"),
    unmanagedSourceDirectories in Compile <+= baseDirectory / "src",
    unmanagedSourceDirectories in Test <+= baseDirectory / "tests",
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += localMavenRepo,
    publishLocalMavenConfiguration <<=
      (packagedArtifacts, deliverLocal, ivyLoggingLevel) map {
        (artifacts, _, level) => 
          new PublishConfiguration(None, localMavenRepo.name, artifacts, level)
      },
    publishLocalMaven <<=
      Classpaths.publishTask(publishLocalMavenConfiguration, deliverLocal))

  val publishSettings = Seq(
    publishTo := Some(
      "Scala Tools Nexus" at
      "http://nexus.scala-tools.org/content/repositories/releases/"))

  lazy val routineer =
    Project("routineer", file(".")) 
      .settings(buildSettings: _*)
      .settings(publishSettings: _*)
      .settings(
         libraryDependencies +=
           "org.specs2" %% "specs2" % "1.5" % "test")
  lazy val scalaz =
    Project("routineer-scalaz", file("scalaz"))
      .settings(buildSettings: _*)
      .settings(publishSettings: _*)
      .settings(
         libraryDependencies +=
           "org.scalaz" %% "scalaz-core" % "6.0.1")
      .dependsOn(routineer)
  lazy val examples =
    Project("routineer-examples", file("examples"))
      .settings(buildSettings: _*)
      .aggregate(examplesServlet)
      .dependsOn(routineer)
  lazy val examplesServlet =
    Project("routineer-examples-servlet", file("examples/servlet"))
      .settings(buildSettings: _*)
      .settings(webSettings: _*)
      .settings(
         webappResources <<= baseDirectory(d => d / "webapp"),
         libraryDependencies ++= Seq(
           "javax.servlet" % "servlet-api" % "2.5" % "provided",
           "org.eclipse.jetty" % "jetty-webapp" % "7.4.5.v20110725" % "jetty"))
      .dependsOn(routineer)
}

