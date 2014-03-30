import sbt._
import Keys._

object Build extends Build {

  val generateWolfeSource = TaskKey[Seq[File]]("sample-a", "demo key A")



}