package ml.wolfe.fg20

import ml.wolfe.WolfeSpec

/**
 * @author Sebastian Riedel
 */
class SettingsIteratorSpecs extends WolfeSpec {

  import TablePotential._

  val s233 = Array(
    Array(0,0,0),
    Array(0,0,1),
    Array(0,0,2),
    Array(0,1,0),
    Array(0,1,1),
    Array(0,1,2),
    Array(0,2,0),
    Array(0,2,1),
    Array(0,2,2),
    Array(1,0,0),
    Array(1,0,1),
    Array(1,0,2),
    Array(1,1,0),
    Array(1,1,1),
    Array(1,1,2),
    Array(1,2,0),
    Array(1,2,1),
    Array(1,2,2)
  )


  "A setting iterator" should {
    "iterate over all settings" in {
      val target = Array.ofDim[Int](3)
      val dims = Array(2,3,3)
      val obs = Array(false,false,false)
      allSettings(dims,obs)(target) { i =>
        for (j <- 0 until target.length) {
          target(j) should be (s233(i)(j))
        }
      }
    }

    "iterate over all settings consistent with observation" in {
      val target = Array(1,0,2)
      val dims = Array(2,3,3)
      val obs = Array(true,false,true)
      var count = 0
      allSettings(dims,obs)(target) { i =>
        for (j <- 0 until target.length) {
          target(j) should be (s233(i)(j))
        }
        target(0) should be (1)
        target(2) should be (2)
        count += 1
      }
      count should be (3)
    }




  }
  
}
