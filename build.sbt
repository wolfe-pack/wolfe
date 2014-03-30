import sbt._
import Build._
import Keys._

sbtPlugin := true

name := "wolfe"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "IESL Release" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)

publishTo <<= (version) { version: String =>
  val homeniscient = "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at homeniscient + "snapshots/")
  else                                   Some("releases"  at homeniscient + "releases/")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials-homeniscient")

//publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies ++= Seq(
  "net.sf.trove4j" % "trove4j" % "3.0.3",
  "org.scalautils" % "scalautils_2.10" % "2.0",
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "com.nativelibs4java" %% "scalaxy-loops" % "0.3-SNAPSHOT" % "provided",
  "cc.factorie" % "factorie" % "1.0.0-M7",
  "com.github.axel22" %% "scalameter" % "0.4",
  "org.scala-lang" % "scala-compiler" % "2.10.3",
  "org.scala-lang" % "scala-library" % "2.10.3",
  "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT",
  "org.slf4j" % "slf4j-api" % "1.7.6",
  "org.slf4j" % "slf4j-simple" % "1.7.6",
  "com.typesafe" % "scalalogging-slf4j_2.10" % "1.1.0",
"org.scalamacros" % "quasiquotes" % "2.0.0-M3" cross CrossVersion.full
)

generateWolfeSource := Seq.empty

sourceGenerators in Compile <+= generateWolfeSource


