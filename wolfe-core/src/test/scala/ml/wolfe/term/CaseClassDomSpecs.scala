package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class CaseClassDomSpecs extends WolfeSpec {

  import TermImplicits._

  "A case class domain macro" should {

    @domain case class World(rain: Boolean, prob: Double)

    "generate domain arguments named like the case class arguments" in {
      val worlds = World.Dom(bools, doubles)
      worlds.rain should be(bools)
      worlds.prob should be(doubles)
    }

    "generate the appropriate lengths method" in {
      val worlds = World.Dom(bools, doubles)
      worlds.lengths should be(Offsets(discOff = 1, contOff = 1))
    }

    "generate a value given a setting" in {
      val worlds = World.Dom(bools, doubles)
      val setting = new Setting(numDisc = 1, numCont = 1)
      setting.disc(0) = 1
      setting.cont(0) = 0.5
      val value = worlds.toValue(setting, Offsets())
      value should be(World(true, 0.5))
    }

    "generate a setting given a value" in {
      val worlds = World.Dom(bools, doubles)
      val setting = new Setting(numDisc = 1, numCont = 1)
      worlds.copyValue(World(true, 0.5), setting, Offsets())
      setting.disc(0) should be(1)
      setting.cont(0) should be(0.5)
    }

    "fill up zero messages" in {
      val worlds = World.Dom(bools, doubles)
      val msgs = new Msgs(numDisc = 1, numCont = 1)
      worlds.fillZeroMsgs(msgs, Offsets())
      msgs.disc(0).msg(0) should be(0.0)
      msgs.disc(0).msg(1) should be(0.0)
      msgs.cont(0).mean should be(0.0)
    }

    "copy marginals into messages" in {
      val worlds = World.Dom(bools, doubles)
      val msgs = new Msgs(numDisc = 1, numCont = 1)
      val marginals = worlds.Marginals(IndexedSeq(0.2, 0.8), 0.5)
      worlds.copyMarginals(marginals, msgs, Offsets())
      msgs.disc(0).msg(0) should be(0.2)
      msgs.disc(0).msg(1) should be(0.8)
      msgs.cont(0).mean should be(0.5)
    }

    "create one and zero values" in {
      val worlds = World.Dom(bools, doubles)
      worlds.zero should be(World(false, 0.0))
      worlds.one should be(World(true, 1.0))
    }


  }

}
