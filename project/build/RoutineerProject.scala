import sbt._
import java.io.{File, Reader, FileReader}
import java.util.Properties

trait PublishLocalMavenProject extends BasicManagedProject
                                  with MavenStyleScalaPaths {
  override def managedStyle = ManagedStyle.Maven
  val localMavenRepo = Resolver.file("Local Maven Repository",
                                     new File(Resolver.userMavenRoot))

  protected def publishLocalMavenConfiguration =
    new DefaultPublishConfiguration(localMavenRepo, "release", false)
  protected def publishLocalMavenAction =
    publishTask(publishIvyModule, publishLocalMavenConfiguration).
      dependsOn(deliverLocal, makePom)
  lazy val publishLocalMaven: Task = publishLocalMavenAction
}

trait PublishProject extends BasicScalaProject with PublishLocalMavenProject {
  val publishTo = "Scala-Tools Releases Repository" at
                  "http://nexus.scala-tools.org/content/repositories/releases/"

  lazy val sourcesArtifact = Artifact.sources(artifactID)
  override def packageToPublishActions = super.packageToPublishActions ++
                                         Seq(packageSrc)
}

trait CommonProject extends BasicScalaProject with PublishLocalMavenProject {
  override def mainScalaSourcePath: Path = "src"
  override def testScalaSourcePath: Path = "tests"
  override def compileOptions = super.compileOptions ++
                                Seq(Deprecation, Unchecked)
}

class RoutineerProject(info: ProjectInfo) extends DefaultProject(info)
                                             with posterous.Publish
                                             with CommonProject
                                             with PublishProject { routineer =>
  trait SubProject extends BasicScalaProject with CommonProject {
    override def dependencies = super.dependencies ++ Seq(routineer)
  }
  class ScalazProject(info: ProjectInfo) extends DefaultProject(info)
                                            with SubProject
                                            with PublishProject {
    val scalaToolsSnapshots =
      "Scala-Tools Maven2 Snapshots Repository" at
      "http://nexus.scala-tools.org/content/repositories/snapshots"
    val scalaz = "org.scalaz" %% "scalaz-core" % "6.0-SNAPSHOT"
  }
  class ExamplesProject(info: ProjectInfo) extends ParentProject(info) {
    trait ExampleProject extends BasicScalaProject with SubProject
    class ServletExampleProject(info: ProjectInfo)
          extends DefaultWebProject(info)
             with ExampleProject {
      override def webappPath: Path = "webapp"
      val servletApi = "javax.servlet" % "servlet-api" % "2.5" % "provided"
      val jetty7 = "org.eclipse.jetty" % "jetty-webapp" % "7.3.0.v20110203" %
                   "test"
    }

    lazy val servlet = project("servlet", "routineer-examples-servlet",
                               new ServletExampleProject(_))                          
  }

  lazy val scalaz = project("scalaz", "routineer-scalaz",
                            new ScalazProject(_))
  lazy val examples = project("examples", "routineer-examples",
                              new ExamplesProject(_))

  override def dependencies = info.dependencies ++ Nil

  val specs = "org.scala-tools.testing" %% "specs" % "1.6.7.2" % "test"

  for {
    realm <- Some("Sonatype Nexus Repository Manager")
    host <- Some("nexus.scala-tools.org")
    credsPath <- {
        val path = Path.userHome / ".scala-tools.org.credentials"
        if (path.exists && !path.isDirectory)
          Some(path)
        else {
          val path = "project" / "publish.credentials"
          if (path.exists && !path.isDirectory)
            Some(path)
          else
            None
        }
      }
    (userName, password) <- {
        var in: Reader = null
        try {
          in = new FileReader(credsPath.asFile)
          val creds = new Properties
          creds.load(in)
          if (!creds.containsKey("username")) {
            log.error("Username is not specified in " + credsPath)
            None
          } else if (!creds.containsKey("password")) {
            log.error("Password is not specified in " + credsPath)
            None
          } else
            Some((creds.getProperty("username"),
                  creds.getProperty("password")))
        } catch {
          case e: Throwable =>
            log.error("Failed to load credentials from " + credsPath +
                      ": " + e)
            None
        } finally {
          try {
            if (in != null)
              in.close
          } catch {
            case _ =>
          }
        }
      }
  } { 
    log.info("Loaded publishing credentials from " + credsPath)
    Credentials.add(realm, host, userName, password)
  }
}

