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

    "generate marginals given a message" in {
      val worlds = World.Dom(bools, doubles)
      val msgs = new Msgs(numDisc = 1, numCont = 1)
      worlds.fillZeroMsgs(msgs, Offsets())
      msgs.disc(0).msg(0) = 0.8
      msgs.disc(0).msg(1) = 0.2
      msgs.cont(0).mean = 0.5
      val marginals = worlds.toMarginals(msgs,Offsets())
      marginals should be (worlds.Marginals(IndexedSeq(0.8,0.2),0.5))
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

    "create a constant" in {
      val worlds = World.Dom(bools, doubles)
      val const = worlds.const(World(true,0.5))
      const.eval() should be (World(true,0.5))
    }

    "create a constant using toTerm" in {
      implicit val worlds = World.Dom(bools, doubles)
      val const = World(true,0.5).toTerm
      const.rain.eval() should be (true)
    }

    "create a static variable" in {
      val worlds = World.Dom(bools, doubles)
      val x = worlds.variable("x")
      val y = x.rain
      x.eval(World(true,0.5)) should be (World(true,0.5))
      y.eval(World(true,0.5)) should be (true)
    }

    "create nested domains" in {
      import scala.language.existentials //todo: why is this necessary? General problem with sequence domains?
      @domain case class Params(weights:IndexedSeq[Double])
      val params = Params.Dom(seqs(doubles,2))
      val x = params.variable("x")
      val term = x.weights(1)
      term.eval(Params(IndexedSeq(1.0,2.0))) should be (2.0)
    }

    "work with classes defined elsewhere" in {
      import ml.wolfe.FactorieVector
      @domain case class Wrapped(vector:FactorieVector)
      val X = Wrapped.Dom(vectors(2))
      val x = X.variable("x")
      x.eval(Wrapped(vector(1,2))).vector should equal (vector(1,2))
    }

    "return all case class values as iterable" in {
      @domain case class DiscWorld(rain:Boolean, sprinkler:Boolean)
      val worlds = DiscWorld.Dom(bools,bools)
      val result = worlds.toSet
      result should be (Set(DiscWorld(false,false),DiscWorld(false,true),DiscWorld(true,false),DiscWorld(true,true)))
    }

    "Work with nested case classes" in {
      @domain case class A(value:Int)
      @domain case class B(a:A)
      val bs = B.Dom(A.Dom(discrete(1,2)))
      bs.toSet should be (Set(B(A(1)),B(A(2))))
    }

  }

}
