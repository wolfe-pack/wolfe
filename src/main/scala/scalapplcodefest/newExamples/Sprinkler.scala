package scalapplcodefest.newExamples

import scalapplcodefest.Wolfe
import scalapplcodefest.sbt._

/**
 * @author Sebastian Riedel
 */
@Compile
class Sprinkler extends (() => Unit) {

  import Wolfe._
  import math._

  def apply() = {

    case class Data(rain: Boolean, sprinkler: Boolean, wet: Boolean)

    def sampleSpace = all(Data)

    val p_rain =
      Map(true -> 0.2, false -> 0.8)

    val p_sprinkler = Map(
      false -> Map(true -> 0.4, false -> 0.6),
      true -> Map(true -> 0.01, false -> 0.99))

    val p_wet = Map(
      (false, false) -> Map(true -> 0.0, false -> 1.0),
      (false, true) -> Map(true -> 0.8, false -> 0.2),
      (true, false) -> Map(true -> 0.9, false -> 0.1),
      (true, true) -> Map(true -> 0.99, false -> 0.01))

    def p(data: Data) = {
      import data._
      p_rain(rain) * p_sprinkler(rain)(sprinkler) * p_wet(sprinkler, rain)(wet)
    }

    //most likely explanation if the grass is wet
    val mpe = argmax(sampleSpace)(_.wet)(x => log(p(x)))

    println(mpe)


  }
}

object Sprinkler {

  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/newExamples/Sprinkler.scala",
      replacers = List(env => new MPGraphReplacer(env)))
  }

}