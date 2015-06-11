import java.io.IOException

import sbt.Keys._
import sbt._
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
  val buildScalaVersion = "2.11.4"
  val buildSbtVersion = "0.13.7"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-language:existentials"),//, "-Ylog-classpath"), //, "-Yrangepos"?
    shellPrompt := ShellPrompt.buildShellPrompt,
    fork in run := true, //use a fresh JVM for sbt run
    connectInput in run := true //to use readLine after sbt run
  )

  val globalDependencies = libraryDependencies ++= Seq(
    "org.scalautils" % "scalautils_2.11" % "2.1.5",
    "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
    "com.typesafe" % "config" % "1.2.1",
//    "com.github.axel22" %% "scalameter" % "0.4",
//    "com.nativelibs4java" %% "scalaxy-loops" % "0.3-SNAPSHOT" % "provided",
//    "com.nativelibs4java" % "scalaxy-streams_2.11" % "0.3.2",
//    "com.nativelibs4java" %% "scalaxy-loops" % "0.3.3" % "provided",
    //    "org.scala-lang" % "scala-compiler" % "2.10.3",
    //    "org.scala-lang" % "scala-library" % "2.10.3",
    "org.slf4j" % "slf4j-api" % "1.7.6",
    "org.slf4j" % "slf4j-simple" % "1.7.6",
    "org.apache.commons" % "commons-compress" % "1.8",
 //   "com.typesafe" % "scalalogging-slf4j_2.10" % "1.1.0",
    "com.typesafe.scala-logging" % "scala-logging-slf4j_2.11" % "2.1.2",
    "com.google.apis" % "google-api-services-freebase" % "v1-rev31-1.13.2-beta",
    "com.google.api-client" % "google-api-client" % "1.14.1-beta",
    "com.googlecode.json-simple" % "json-simple" % "1.1",
    "org.json4s" %% "json4s-jackson" % "3.2.11",
    "org.mongodb" %% "casbah" % "2.7.4",
    "org.scala-lang.modules" %% "scala-pickling" % "0.10.0"


 //   "edu.illinois.cs.cogcomp" % "IllinoisNerExtended" % "2.7"


    //    "net.liftweb" %% "lift-json" % "2.3"
  )

  val utilDependencies = libraryDependencies ++= Seq(
    "cc.factorie" % "factorie_2.11" % "1.1"
//    "org.scalamacros" %% "quasiquotes" % "2.0.0"

  )

  val coreDependencies = libraryDependencies ++= Seq(
    "net.sf.trove4j" % "trove4j" % "3.0.3",
    "cc.factorie" % "factorie_2.11" % "1.1",
//    "org.scalamacros" %% "quasiquotes" % "2.0.0",
    "org.scalanlp" %% "breeze" % "0.8.1",
    //"org.scalanlp" %% "breeze-natives" % "0.8.1",
    "org.sameersingh.htmlgen" % "htmlgen" % "0.3"
    //    "org.scalanlp" %% "breeze-math" % "0.3",
    //    "org.scalanlp" %% "breeze-learn" % "0.3",
    //    "org.scalanlp" %% "breeze-process" % "0.3",
    //    "org.scalanlp" %% "breeze-viz" % "0.3"
  )

  val nlpDependencies = libraryDependencies ++= Seq(
    "edu.arizona.sista" % "processors" % "3.3"
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
      //incOptions := incOptions.value.withNameHashing(false),
      initialCommands := """
        import ml.wolfe.term.TermImplicits
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

  lazy val berkNER = ProjectRef(uri("git://github.com/gregdurrett/berkeley-entity.git"), "berkeley-entity")
  lazy val jamr = ProjectRef(uri("git://github.com/jflanigan/jamr.git"), "jamr")
//  lazy val word2vec = ProjectRef(uri("git://github.com/trananh/word2vec-scala.git"), "word2vec-scala")

  lazy val macroSettings = Seq(
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M1" cross CrossVersion.full)
  )

  lazy val root = Project(
    id = "wolfe",
    base = file("."),
    settings = Project.defaultSettings ++ publishSettings ++ generalSettings ++ releaseSettings
  ) aggregate(core, nlp, examples, ui, util)

  lazy val core = Project(
    id = "wolfe-core",
    base = file("wolfe-core"),
    settings = buildSettings ++ globalSettings ++ coreDependencies ++ macroSettings) dependsOn util % "test->test;compile->compile"

  lazy val nlp = Project(
    id = "wolfe-nlp",
    base = file("wolfe-nlp"),
    settings = buildSettings ++ globalSettings ++ nlpDependencies
  ) dependsOn (
  core % "test->test;compile->compile"
//  word2vec % "test->test;compile->compile"
  )

  lazy val util = Project(
    id = "wolfe-util",
    base = file("wolfe-util"),
    settings = buildSettings ++ globalSettings ++ utilDependencies ++ macroSettings
  )


  lazy val examples = Project(
    id = "wolfe-examples",
    base = file("wolfe-examples"),
    settings = buildSettings ++ globalSettings ++ macroSettings
  ) dependsOn(
  core % "test->test;compile->compile",
  nlp % "test->test;compile->compile",
  util % "test->test;compile->compile"
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