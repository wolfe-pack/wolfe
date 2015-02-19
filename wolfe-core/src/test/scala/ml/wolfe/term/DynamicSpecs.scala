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
    "be composable through the map operator" in {
      val dyn = Dynamic.sequential(0 until 5)
      val composed = for (i <- dyn) yield i + 5
      for (i <- 0 until 5){
        dyn.updateValue()
        composed.value() should be (i + 5)
        composed.value() should be (i + 5)
      }

    }

    "be composable through the flatmap operator" in {
      val dyn = Dynamic.sequential(0 until 5)
      val composed = for (i <- dyn; j <- dyn) yield i + j
      for (i <- 0 until 5){
        dyn.updateValue()
        composed.value() should be (i * 2)
        composed.value() should be (i * 2)
      }
      dyn.childCount should be (2)
    }

    "be composable with other dynamic objects" in {
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

    "support nested maps and flatmaps" in {
      val n = 10
      val data = for (i <- 0 until n; j <- 0 until n; k <- 0 until n) yield (i, j + n, k + 2 * n)
      val dyn0 = Dynamic.sequential(data)
      val dyn1 = dyn0.map(_._1)
      val dyn2 = dyn0.map(_._2)
      val dyn3 = dyn0.map(_._3)

      val dyn5 = dyn1.flatMap(i1 => dyn2.flatMap(i2 => dyn3.map(i3 => i1 + i2 + i3)))

      for (i <- 0 until n) {
        dyn0.updateValue()
        dyn5.value() should be (dyn1.value() + dyn2.value() + dyn3.value())
        dyn0.descendentCount should be (7)
      }



    }

  }

}
