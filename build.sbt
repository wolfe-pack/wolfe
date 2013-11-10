name := "scalapplcodefest"

version := "0.1.0"

scalaVersion := "2.10.2"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
     "net.sf.trove4j" % "trove4j" % "3.0.3",
     "com.nativelibs4java" %% "scalaxy-loops" % "0.3-SNAPSHOT" % "provided"
)

