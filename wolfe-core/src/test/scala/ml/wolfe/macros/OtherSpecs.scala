package ml.wolfe.macros

import ml.wolfe.{BeliefPropagation, FactorGraph, Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class OtherSpecs extends WolfeSpec {

  import Wolfe._
  import OptimizedOperators._

  "A Factor Graph Store" should {
    "store the factor graph used in inference" in {
      @OptimizeByInference(FactorGraph.Store andThen BeliefPropagation.maxProduct(1))
      def model(coin: Boolean) = bernoulli(0.9)(coin)
      val result = argmax(bools) { model }
      val fg = FactorGraph.Store.factorGraph
      result should be (true)
      fg.factors.size should be (1)
      fg.nodes.size should be (1)
    }
  }

}
