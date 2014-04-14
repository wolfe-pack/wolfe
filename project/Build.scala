import java.io.IOException
import sbt._
import sbt.Keys._
import sbtrelease.ReleasePlugin._


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
  val buildScalaVersion = "2.10.3"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    shellPrompt := ShellPrompt.buildShellPrompt,
    fork in run := true //use a fresh JVM for sbt run
  )

  val globalDependencies = libraryDependencies ++= Seq(
    "org.scalautils" % "scalautils_2.10" % "2.0",
    "org.scalatest" %% "scalatest" % "2.1.0" % "test",
    "com.github.axel22" %% "scalameter" % "0.4",
    "org.scala-lang" % "scala-compiler" % "2.10.3",
    "org.scala-lang" % "scala-library" % "2.10.3",
    "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT",
    "org.slf4j" % "slf4j-api" % "1.7.6",
    "org.slf4j" % "slf4j-simple" % "1.7.6",
    "org.apache.commons" % "commons-compress" % "1.8",
    "com.typesafe" % "scalalogging-slf4j_2.10" % "1.1.0"
  )

  val coreDependencies = libraryDependencies ++= Seq(
    "net.sf.trove4j" % "trove4j" % "3.0.3",
    "com.nativelibs4java" %% "scalaxy-loops" % "0.3-SNAPSHOT" % "provided",
    "cc.factorie" % "factorie" % "1.0.0-M7",
    "org.scalamacros" %% "quasiquotes" % "2.0.0-M3" cross CrossVersion.full
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


  val globalSettings =
    Seq(
      scalacOptions ++= Seq("-feature"),
      resolvers ++= Seq(
        "IESL Release" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
        Resolver.sonatypeRepo("snapshots"),
        Resolver.sonatypeRepo("releases")
      ),
      globalDependencies,
      initialCommands := """
        import ml.wolfe.Wolfe._
        import ml.wolfe.macros.OptimizedOperators._
      """
    ) ++ releaseSettings ++ publishSettings

}


object Build extends Build {

  import BuildSettings._

  lazy val root = Project(
    id = "wolfe",
    base = file("."),
    settings = publishSettings
  ) aggregate(core, examples)

  lazy val core = Project(
    id = "wolfe-core",
    base = file("wolfe-core"),
    settings = buildSettings ++ globalSettings ++ coreDependencies ++ Seq(
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)
    )
  )

  lazy val examples = Project(
    id = "wolfe-examples",
    base = file("wolfe-examples"),
    settings = buildSettings ++ globalSettings
  ) dependsOn core % "test->test;compile->compile"


}