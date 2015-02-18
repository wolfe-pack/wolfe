package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class DynamicSpecs extends WolfeSpec {

  "A dynamic object" should {
    "evaluate to different values after updates, and the same values without updates" in {
      val dyn = Dynamic.sequential(0 until 5)
      for (i <- 0 until 5){
        dyn.updateValue()
        dyn.value() should be (i)
        dyn.value() should be (i)
      }
    }
    "should be composable through the map operator" in {
      val dyn = Dynamic.sequential(0 until 5)
      val composed = for (i <- dyn) yield i + 5
      for (i <- 0 until 5){
        dyn.updateValue()
        composed.value() should be (i + 5)
        composed.value() should be (i + 5)
      }

    }

    "should be composable through the flatmap operator" in {
      val dyn = Dynamic.sequential(0 until 5)
      val composed = for (i <- dyn; j <- dyn) yield i + j
      for (i <- 0 until 5){
        dyn.updateValue()
        composed.value() should be (i * 2)
        composed.value() should be (i * 2)
      }
      dyn.childCount should be (2)
    }

    "should be composable with other dynamic objects" in {
      val dyn1 = Dynamic.sequential(0 until 5)
      val dyn2 = Dynamic.sequential(0 until 5)

      val composed = for (i1 <- dyn1; i2 <- dyn2) yield i1 + i2
      for (i <- 0 until 5) {
        dyn1.updateValue()
        for (j <- 0 until 5) {
          dyn2.updateValue()
          composed.value() should  be (i + j)
          composed.value() should  be (i + j)
        }
      }
      dyn1.childCount should be (1)
      dyn2.childCount should be (1)


    }


  }

}
