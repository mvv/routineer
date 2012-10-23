addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.7")

libraryDependencies <+= sbtVersion { v =>
  "com.github.siasia" %% "xsbt-web-plugin" % (v + "-0.2.11.1")
}

