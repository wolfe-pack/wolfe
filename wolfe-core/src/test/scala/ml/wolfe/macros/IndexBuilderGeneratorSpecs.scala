package ml.wolfe.macros

import ml.wolfe.{Wolfe, WolfeSpec}
import Wolfe._

/**
 * @author Sebastian Riedel
 */
class IndexBuilderGeneratorSpecs extends WolfeSpec {

  "An index builder generator" should {
    "generate an index builder based on a static dot product" in {
      val f = () => (w: Vector) => w dot oneHot(1 -> 2 -> 3)
      //val builder = generateBuilder(() => (w:Vector) => )
    }
    "generate an index builder based on a sum over max terms" in {
      import OptimizedOperators._
      val data = Range(0,5)
      val space = Range(0,5)
      val f = () => (w: Vector) => sum{ over(data) of (d => max { over(0 until 4) of (i => w dot oneHot(i -> (i + 1)))}) }
      //val builder = generateBuilder(() => (w:Vector) => )
    }
  }


}
