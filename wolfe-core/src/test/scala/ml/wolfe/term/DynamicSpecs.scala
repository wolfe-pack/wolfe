package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class DynamicSpecs extends WolfeSpec {

  "A dynamic object" should {
    "evaluate to different values after updates, and the same values without updates" in {
      val dyn = Dynamic2.sequential(0 until 5)
      dyn.updateValue()
      dyn.value() should be (0)
      dyn.value() should be (0)
      dyn.updateValue()
      dyn.value() should be (1)
      dyn.value() should be (1)
    }
    "should be composable through the map operator" in {
      val dyn = Dynamic2.sequential(0 until 5)
      val composed = for (i <- dyn) yield i + 5
      dyn.updateValue()
      composed.value() should be (5)
      composed.value() should be (5)
      dyn.updateValue()
      composed.value() should be (6)
      composed.value() should be (6)

    }

    "should be composable through the flatmap operator" in {
      val dyn = Dynamic2.sequential(0 until 5)
      val composed = for (i <- dyn; j <- dyn) yield i + j
      dyn.updateValue()
      composed.value() should be (0)
      composed.value() should be (0)
      dyn.updateValue()
      composed.value() should be (2)
      composed.value() should be (2)
    }


  }

}
