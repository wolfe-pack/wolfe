resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.3")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.2")

//addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "0.99.7.1")

//addSbtPlugin("com.sksamuel.scoverage" %% "sbt-coveralls" % "0.0.5")

addSbtPlugin("org.scala-sbt.plugins" % "sbt-onejar" % "0.8")