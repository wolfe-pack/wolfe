import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

val map = new mutable.HashMap[String,Array[String]]

map("rel") = map.getOrElse("rel",Array.empty[String]) :+ "parent"

val map2 = new mutable.HashMap[String,ArrayBuffer[String]]

map2.getOrElse("rel",new ArrayBuffer[String]) += "parent"

val map3 = new mutable.HashMap[String,List[String]] withDefaultValue Nil

map3("rel") = "parent" :: map3("rel")

map3("rel") = "parent2" :: map3("rel")

map3.mkString("\n")


