package ml.wolfe.term

import ml.wolfe.{SimpleFeatureIndex, WolfeSpec}

/**
 * @author riedel
 */
class FeatureIndexSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A feature index" should {

    "provide an inverse mapping" in {
      val dom = Vectors(100000)
      val index = new SimpleFeatureIndex()(dom)
      val b1 = Bools.Var
      val b2 = Bools.Var
      val i3 = Ints(0 until 5).Var
      val t = index.oneHot('f1, b1, b2, i3) + index.oneHot('f2, true, b1, i3)
      val v = t eval(b1 := true, b2 := false, i3 := 3)
      index.toMap(v) should be(Map(IndexedSeq('f1, true, false, 3) -> 1.0, IndexedSeq('f2, true, 3) -> 1.0))
    }


  }


}
