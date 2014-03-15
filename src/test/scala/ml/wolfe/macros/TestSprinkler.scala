package ml.wolfe.macros

import ml.wolfe.WolfeEnv._
import OptimizedWolfe._
import scala.math._
import ml.wolfe.util.LoggerUtil

/**
 * @author Sebastian Riedel
 */
object TestSprinkler {

  def main(args: Array[String]) {

    case class Data(rain: Boolean, sprinkler: Boolean, wet: Boolean)

    val sampleSpace = all(Data)(c(bools, bools, bools))

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

    def p(x: Data) = p_rain(x.rain) * p_sprinkler(x.rain)(x.sprinkler) * p_wet(x.sprinkler, x.rain)(x.wet)


    LoggerUtil.info("Sample space: " + sampleSpace.size)

    //most likely explanation if the grass is wet
    val expected = BruteForceWolfe.argmax(sampleSpace)(_.wet)(x => log(p(x)))
    val actual = OptimizedWolfe.argmax(sampleSpace)(_.wet)(x => log(p(x)))

    LoggerUtil.info("Expected: " + expected)
    LoggerUtil.info("Actual: " + actual)


  }
}
