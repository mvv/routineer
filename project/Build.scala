import sbt._
import Keys._
import com.github.siasia.WebPlugin.webSettings
import com.github.siasia.PluginKeys.webappResources

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
    version := "0.1.2",
    scalaVersion := "2.10.0",
    scalacOptions += "-deprecation",
    crossScalaVersions := Seq("2.8.1", "2.9.0", "2.9.0-1",
                              "2.9.1", "2.9.1-1", "2.9.2",
                              "2.10.0"),
    scalaSource in Compile <<= baseDirectory / "src",
    scalaSource in Test <<= baseDirectory / "tests",
    unmanagedSourceDirectories in Compile <<= Seq(scalaSource in Compile).join,
    unmanagedSourceDirectories in Test <<= Seq(scalaSource in Test).join,
    publishArtifact in Test := false,
    resolvers += localMavenRepo,
    publishLocalMavenConfiguration <<=
      (packagedArtifacts, deliverLocal, checksums in publishLocal,
       ivyLoggingLevel) map {
        (artifacts, _, chsums, level) => 
          new PublishConfiguration(
                None, localMavenRepo.name, artifacts, chsums, level)
      },
    publishLocalMaven <<=
      Classpaths.publishTask(publishLocalMavenConfiguration, deliverLocal))

  val publishSettings = Seq(
    publishMavenStyle := true,
    publishTo := Some(
      "releases" at
      "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
    pomIncludeRepository := { _ => false },
    pomExtra := (
      <url>http://github.com/mvv/routineer</url>
      <licenses>
        <license>
          <name>Apache License, Version 2.0</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:mvv/routineer.git</url>
        <connection>scm:git:git@github.com:mvv/routineer.git</connection>
      </scm>
      <developers>
        <developer>
          <id>mvv</id>
          <name>Mikhail Vorozhtsov</name>
          <url>http://github.com/mvv</url>
        </developer>
      </developers>),
    pomPostProcess := { pom =>
      import scala.xml.transform.{RewriteRule, RuleTransformer}
      val dropTestDeps = new RewriteRule {
        override def transform(n: scala.xml.Node) =
          if (n.label == "dependency" && (n \ "scope").text == "test")
            Seq.empty
          else
            Seq(n)
      }
      new RuleTransformer(dropTestDeps)(pom)
    })

  lazy val routineer =
    Project("routineer", file(".")) 
      .settings(buildSettings: _*)
      .settings(publishSettings: _*)
      .settings(
         resolvers <++= scalaVersion {
           case v if v.startsWith("2.8.") =>
             Seq("sonatype-snapshots" at
                   "http://oss.sonatype.org/content/repositories/snapshots")
           case _ => Seq.empty
         },
         libraryDependencies <+= scalaVersion { v =>
           val v1 = if (v.startsWith("2.8.") || v.startsWith("2.9.0")) "1.5"
                    else if (v.startsWith("2.10")) "1.13"
                    else "1.12.3"
           "org.specs2" %% "specs2" % v1 % "test"
         })
  lazy val scalaz =
    Project("routineer-scalaz", file("scalaz"))
      .settings(buildSettings: _*)
      .settings(publishSettings: _*)
      .settings(
         libraryDependencies += 
           "org.scalaz" % "scalaz-core" % "6.0.4" cross
             CrossVersion.binaryMapped {
               case "2.9.0" => "2.9.0-1"
               case "2.9.1-1" => "2.9.1"
               case "2.10.0" => "2.10.0-RC5"
               case v => v
             })
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
         webappResources in Compile <<=
           baseDirectory { d => Seq(d / "webapp") },
         classpathTypes += "orbit", 
         libraryDependencies ++= Seq(
           "javax.servlet" % "servlet-api" % "2.5" % "provided",
           "org.eclipse.jetty.orbit" % "javax.servlet" %
             "3.0.0.v201112011016" % "container"
               artifacts Artifact("javax.servlet", "jar", "jar"),
           "org.eclipse.jetty" % "jetty-webapp" %
             "8.1.7.v20120910" % "container"))
      .dependsOn(routineer)
}

