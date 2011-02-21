import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val snuggletexRepo = "snuggletex" at "http://www2.ph.ed.ac.uk/maven2"
  val tristanhuntRepo = "tristanhunt" at
                        "http://tristanhunt.com:8081/content/groups/public/"
  val posterous = "net.databinder" % "posterous-sbt" % "0.1.6"
}

