import sbt._

trait CommonProject extends BasicScalaProject {
  override def compileOptions = super.compileOptions ++
                                Seq(Deprecation, Unchecked)
}

class RoutineerProject(info: ProjectInfo) extends DefaultProject(info)
                                             with CommonProject { routineer =>
  class ExamplesProject(info: ProjectInfo) extends ParentProject(info) {
    trait ExampleProject extends BasicScalaProject with CommonProject {
      override def dependencies = super.dependencies ++ Seq(routineer)
    }

    class ServletExampleProject(info: ProjectInfo)
          extends DefaultWebProject(info)
                  with ExampleProject {
      val servletApi = "javax.servlet" % "servlet-api" % "2.5" % "provided"
      val jetty7 = "org.eclipse.jetty" % "jetty-webapp" % "7.3.0.v20110203" %
                   "test"
    }

    lazy val servlet = project("servlet", "routineer-examples-servlet",
                               new ServletExampleProject(_))                          
  }

  lazy val examples = project("examples", "routineer-examples",
                              new ExamplesProject(_))

  override def dependencies = info.dependencies ++ Nil

  val specs = "org.scala-tools.testing" %
              ("specs_" + crossScalaVersionString) %
              "1.6.7.2" % "test"
}

