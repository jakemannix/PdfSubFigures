logLevel := Level.Warn

lazy val ai2PluginsVersion = "1.0.1"

addSbtPlugin("org.allenai.plugins" % "allenai-sbt-plugins" % ai2PluginsVersion)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.12.0")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")

// This should be in local dev's ~/.sbt/0.13/plugins/ and not a plugin for the project:
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.5")