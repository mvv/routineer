import sbt._

trait CommonProject extends BasicScalaProject with MavenStyleScalaPaths {
  override def mainScalaSourcePath: Path = "src"
  override def testScalaSourcePath: Path = "tests"
  override def compileOptions = super.compileOptions ++
                                Seq(Deprecation, Unchecked)
}

class RoutineerProject(info: ProjectInfo) extends DefaultProject(info)
                                             with posterous.Publish
                                             with CommonProject { routineer =>
  trait SubProject extends BasicScalaProject with CommonProject {
    override def dependencies = super.dependencies ++ Seq(routineer)
  }
  class ScalazProject(info: ProjectInfo) extends DefaultProject(info)
                                            with SubProject {
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
}

