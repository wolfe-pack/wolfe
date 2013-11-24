name := "scalapplcodefest"

version := "0.1.0"

scalaVersion := "2.10.2"

resolvers ++= Seq(
    "IESL Release" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
    Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
     "net.sf.trove4j" % "trove4j" % "3.0.3",
     "org.scalatest" %% "scalatest" % "2.0" % "test",
     "com.nativelibs4java" %% "scalaxy-loops" % "0.3-SNAPSHOT" % "provided",
      "cc.factorie" % "factorie" % "1.0.0-M7"
)

