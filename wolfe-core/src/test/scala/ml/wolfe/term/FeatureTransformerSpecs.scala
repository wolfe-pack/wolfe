package ml.wolfe.term

import ml.wolfe._

/**
 * @author riedel
 */
class FeatureTransformerSpecs extends WolfeSpec {

  import TermImplicits._
  import Transformer._

  "A feature aggregator" should {
    "aggregate sums of feature terms into a feature sum" in {
      implicit val Thetas = Vectors(10)
      implicit val index = new SimpleFeatureIndex(Thetas)
      val b1 = Bools.Var
      val b2 = Bools.Var
      val f1 = feature('b1,b1)
      val f2 = feature('b2,b2)
      val f3 = feature('b1b2, b1 && b2)
      val t = f1 + f2 + f3
      val result = FeatureTransformer.aggregateFeatures(Transformer.clean(t))
      val expected = FeatureSum(Seq(f1,f2,f3))
      result should be (expected)
    }
  }

}
