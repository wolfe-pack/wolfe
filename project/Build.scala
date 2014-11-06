import java.io.IOException
import sbt._
import sbt.Keys._
import sbtrelease.ReleasePlugin._
//import scoverage.ScoverageSbtPlugin._
import com.github.retronym.SbtOneJar.oneJarSettings

object ShellPrompt {
  object devnull extends ProcessLogger {
    def info(s: => String) {}
    def error(s: => String) {}
    def buffer[T](f: => T): T = f
  }
  def currBranch = {
    try {
      (
      ("git status -sb" lines_! devnull headOption)
      getOrElse "-" stripPrefix "## "
      )
    } catch {
      case ex: IOException => "?"
    }
  }

  val buildShellPrompt = {
    (state: State) => {
      val extracted = Project.extract(state)
      val currProject = extracted.currentProject.id
      "%s:%s:%s> ".format(
        currProject, currBranch, extracted.get(version)
      )
    }
  }
}

object BuildSettings {
  val buildName         = "wolfe"
  val buildOrganization = "ml.wolfe"
  val buildScalaVersion = "2.10.4"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"), //, "-Yrangepos"?
    shellPrompt := ShellPrompt.buildShellPrompt,
    fork in run := true //use a fresh JVM for sbt run
  )

  val globalDependencies = libraryDependencies ++= Seq(
    "org.scalautils" % "scalautils_2.10" % "2.0",
    "org.scalatest" %% "scalatest" % "2.1.0" % "test",
    "com.typesafe" % "config" % "1.2.1",
    "com.github.axel22" %% "scalameter" % "0.4",
    "com.nativelibs4java" %% "scalaxy-loops" % "0.3-SNAPSHOT" % "provided",
//    "org.scala-lang" % "scala-compiler" % "2.10.3",
//    "org.scala-lang" % "scala-library" % "2.10.3",
    "org.slf4j" % "slf4j-api" % "1.7.6",
    "org.slf4j" % "slf4j-simple" % "1.7.6",
    "org.apache.commons" % "commons-compress" % "1.8",
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.2.3",
    "com.typesafe" % "scalalogging-slf4j_2.10" % "1.1.0",
    "com.google.apis" % "google-api-services-freebase" % "v1-rev31-1.13.2-beta",
    "com.google.api-client" % "google-api-client" % "1.14.1-beta",
    "com.googlecode.json-simple" % "json-simple" % "1.1",
    "org.json4s" %% "json4s-native" % "3.2.10",
    "org.mongodb" %% "casbah" % "2.5.0"


  //    "net.liftweb" %% "lift-json" % "2.3"
  )

  val coreDependencies = libraryDependencies ++= Seq(
    "net.sf.trove4j" % "trove4j" % "3.0.3",
    "cc.factorie" % "factorie" % "1.0",
    "org.scalamacros" %% "quasiquotes" % "2.0.0",
    "org.scalanlp" %% "breeze" % "0.8.1",
    "org.scalanlp" %% "breeze-natives" % "0.8.1",
    "org.sameersingh.htmlgen" % "htmlgen" % "0.1.1"
    //    "org.scalanlp" %% "breeze-math" % "0.3",
    //    "org.scalanlp" %% "breeze-learn" % "0.3",
    //    "org.scalanlp" %% "breeze-process" % "0.3",
    //    "org.scalanlp" %% "breeze-viz" % "0.3"
  )

  val nlpDependencies = libraryDependencies ++= Seq(
    "edu.arizona.sista" % "processors" % "2.0",
    "org.scala-lang" %% "scala-pickling" % "0.8.0"
  )

  val appDependencies = libraryDependencies ++= Seq(
    //"pl.project13.scala" %% "rainbow" % "0.2"
  )

  val uiDependencies = libraryDependencies ++= Seq(
    "eu.henkelmann" % "actuarius_2.10.0" % "0.2.6"
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
      initialCommands := """
        import ml.wolfe.Wolfe._
        import ml.wolfe.macros.OptimizedOperators._
                         """
    )

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
        Resolver.sonatypeRepo("snapshots"),
        Resolver.sonatypeRepo("releases")
      ),
      globalDependencies
    ) ++ generalSettings ++ releaseSettings ++ publishSettings ++  oneJarSettings //++ coverallsSettings ++ instrumentSettings

//  ScoverageKeys.excludedPackages in ScoverageCompile := ".*;.*.*"
}


object Build extends Build {

  import BuildSettings._


  lazy val root = Project(
    id = "wolfe",
    base = file("."),
    settings = Project.defaultSettings ++ publishSettings ++ generalSettings ++ releaseSettings
  ) aggregate(core, nlp, examples, apps, ui, neural, util)

  lazy val core = Project(
    id = "wolfe-core",
    base = file("wolfe-core"),
    settings = buildSettings ++ globalSettings ++ coreDependencies ++ Seq(
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full)
    )
  )

  lazy val nlp = Project(
    id = "wolfe-nlp",
    base = file("wolfe-nlp"),
    settings = buildSettings ++ globalSettings ++ nlpDependencies
  ) dependsOn core % "test->test;compile->compile"
  
  lazy val neural = Project(
    id = "wolfe-neural",
    base = file("wolfe-neural"),
    settings = buildSettings ++ globalSettings
  ) dependsOn core % "test->test;compile->compile"

  lazy val util = Project(
    id = "wolfe-util",
    base = file("wolfe-util"),
    settings = buildSettings ++ globalSettings
  ) dependsOn core % "test->test;compile->compile"


  lazy val examples = Project(
    id = "wolfe-examples",
    base = file("wolfe-examples"),
    settings = buildSettings ++ globalSettings
  ) dependsOn(
  core % "test->test;compile->compile",
  nlp % "test->test;compile->compile"
  )

  lazy val apps = Project(
    id = "wolfe-apps",
    base = file("wolfe-apps"),
    settings = buildSettings ++ globalSettings ++ appDependencies
  ) dependsOn(
  core % "test->test;compile->compile",
  nlp % "test->test;compile->compile"
  )

  lazy val ui = Project(
    id = "wolfe-ui",
    base = file("wolfe-ui"),
    settings = buildSettings ++ globalSettings ++ uiDependencies
  ) dependsOn(
  core % "test->test;compile->compile",
  nlp % "test->test;compile->compile"
  )

}