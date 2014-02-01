package scalapplcodefest.macros

import scalapplcodefest.WolfeEnv._
import OptimizedWolfe._
import scala.math._

/**
 * @author Sebastian Riedel
 */
object TestSprinkler {

  def main(args: Array[String]) {

    case class Data(rain: Boolean, sprinkler: Boolean, wet: Boolean)

    val sampleSpace = all(Data)(c(bools,bools,bools))

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


    println(sampleSpace)

    //most likely explanation if the grass is wet
    val expected = BruteForceWolfe.argmax(sampleSpace)(_.wet)(x => log(p(x)))
    val actual = OptimizedWolfe.argmax(sampleSpace)(_.wet)(x => log(p(x)))

    println(expected)
    println(actual)


  }
}
