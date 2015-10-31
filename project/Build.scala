import java.io.IOException

import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin._

//import scoverage.ScoverageSbtPlugin._

import com.github.retronym.SbtOneJar.oneJarSettings


object BuildSettings {
  val buildName         = "wolfe"
  val buildOrganization = "ml.wolfe"
  val buildScalaVersion = "2.11.7"
  val buildSbtVersion = "0.13.7"

  val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    scalacOptions := Seq(
      "-unchecked", "-deprecation",
      "-optimise", "-Yclosure-elim", "-Yinline",
      "-feature", "-language:existentials"),//, "-Ylog-classpath"), //, "-Yrangepos"?
    fork in run := true, //use a fresh JVM for sbt run
    connectInput in run := true //to use readLine after sbt run
  )

  val globalDependencies = libraryDependencies ++= Seq(
    "org.scalautils" % "scalautils_2.11" % "2.1.5",
    "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
    "com.typesafe" % "config" % "1.2.1",
    "org.slf4j" % "slf4j-api" % "1.7.10",
    "org.slf4j" % "slf4j-simple" % "1.7.10",
    "org.apache.commons" % "commons-compress" % "1.8",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "org.json4s" %% "json4s-jackson" % "3.2.11",
//    "org.scala-lang.modules" %% "scala-pickling" % "0.10.0",
    "com.nativelibs4java" %% "scalaxy-loops" % "0.3.3" % "provided",
    "com.nativelibs4java" %% "scalaxy-streams" % "0.3.4" % "provided"
  )

  val coreDependencies = libraryDependencies ++= Seq(
    "net.sf.trove4j" % "trove4j" % "3.0.3",
    //"org.nd4j" % "nd4s_2.12.0-M1" % "0.4-rc3",
    "org.nd4j" % "nd4j-api" % "0.4-rc3.5",
    "org.nd4j" % "nd4j-jblas" % "0.4-rc3.5",
    "org.jblas" % "jblas" % "1.2.4",
    "org.nd4j" % "jcublas" % "6.5",
    "org.zeromq" % "jeromq" % "0.3.5",
    "org.scalanlp" %% "breeze" % "0.11.2"
//    "org.scalanlp" %% "breeze-natives" % "0.11.2",
//    "org.scalanlp" %% "breeze-viz" % "0.11.2"

    //"org.scalanlp" %% "breeze-natives" % "0.8.1",
  )

  val publishSettings = Seq(
    publishTo <<= version {
      version: String =>
        val homeniscient = "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/"
        if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at homeniscient + "snapshots/")
        else Some("releases" at homeniscient + "releases/")
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials-homeniscient")
  )

  val generalSettings =
    Seq(
      //incOptions := incOptions.value.withNameHashing(false),
      initialCommands := """
        import ml.wolfe._
        import Language._
                         """
    ) ++ net.virtualvoid.sbt.graph.Plugin.graphSettings

  def vmargs = Command.args("vmargs", "<name>") {
    (state, args) =>
      val javaRunOptions = args.mkString(" ")
      println("Applying JVM arguments: " + javaRunOptions)
      Project.extract(state).append(javaOptions := Seq(javaRunOptions), state)
  }

  val globalSettings =
    Seq(
      commands ++= Seq(vmargs),
      scalacOptions ++= Seq("-feature"),
      resolvers ++= Seq(
        "IESL Release" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
        Resolver.mavenLocal,
        Resolver.defaultLocal,
        Resolver.sonatypeRepo("snapshots"),
        Resolver.sonatypeRepo("releases"),
        "UIUC Releases" at "http://cogcomp.cs.illinois.edu/m2repo"
      ),
      globalDependencies
    ) ++ generalSettings ++ releaseSettings ++ publishSettings ++ oneJarSettings //++ coverallsSettings ++ instrumentSettings

  //  ScoverageKeys.excludedPackages in ScoverageCompile := ".*;.*.*"
}


object Build extends Build {

  import BuildSettings._

  lazy val macroSettings = Seq(
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-SNAPSHOT" cross CrossVersion.full)
  )

  lazy val root = Project(
    id = "wolfe",
    base = file("."),
    settings = buildSettings ++ publishSettings ++ generalSettings ++ releaseSettings
  ) aggregate(core) dependsOn (core)

  lazy val core = Project(
    id = "wolfe-core",
    base = file("wolfe-core"),
    settings = buildSettings ++ globalSettings ++ coreDependencies ++ macroSettings)


}