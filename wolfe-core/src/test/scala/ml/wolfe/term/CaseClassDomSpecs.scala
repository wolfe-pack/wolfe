package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class CaseClassDomSpecs extends WolfeSpec {

  import TermImplicits._

  "A case class domain macro" should {

    @domain case class World(rain:Boolean,prob:Double)

    "generate domain arguments named like the case class arguments" in {
      val worlds = World.Dom(bools,doubles)
      worlds.rain should be (bools)
      worlds.prob should be (doubles)
    }

    "generate the appropriate lengths method" in {
      val worlds = World.Dom(bools,doubles)
      worlds.lengths should be (Offsets(discOff = 1, contOff = 1))
    }

    "generate a value given a setting" in {
      val worlds = World.Dom(bools,doubles)
      val setting = new Setting(numDisc = 1, numCont = 1)
      setting.disc(0) = 1
      setting.cont(0) = 0.5
      val value = worlds.toValue(setting,Offsets())
      value should be (World(true,0.5))
    }



  }

}
