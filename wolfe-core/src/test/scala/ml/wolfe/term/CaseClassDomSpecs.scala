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
      val worlds = World.Values(Bools, Doubles)
      worlds.rain should be(Bools)
      worlds.prob should be(Doubles)
    }

    "generate the appropriate lengths method" in {
      val worlds = World.Values(Bools, Doubles)
      worlds.lengths should be(Offsets(discOff = 1, contOff = 1))
    }

    "generate a value given a setting" in {
      val worlds = World.Values(Bools, Doubles)
      val setting = new Setting(numDisc = 1, numCont = 1)
      setting.disc(0) = 1
      setting.cont(0) = 0.5
      val value = worlds.toValue(setting, Offsets())
      value should be(World(true, 0.5))
    }

    "generate marginals given a message" in {
      val worlds = World.Values(Bools, Doubles)
      val msgs = new Msg(numDisc = 1, numCont = 1)
      worlds.fillZeroMsg(msgs, Offsets())
      msgs.disc(0).msg(0) = 0.8
      msgs.disc(0).msg(1) = 0.2
      msgs.cont(0).mean = 0.5
      val marginals = worlds.toMarginals(msgs,Offsets())
      marginals should be (worlds.Marginals(Map(false -> 0.8, true -> 0.2),0.5))
    }

    "generate a setting given a value" in {
      val worlds = World.Values(Bools, Doubles)
      val setting = new Setting(numDisc = 1, numCont = 1)
      worlds.copyValue(World(true, 0.5), setting, Offsets())
      setting.disc(0) should be(1)
      setting.cont(0) should be(0.5)
    }


    "fill up zero messages" in {
      val worlds = World.Values(Bools, Doubles)
      val msgs = new Msg(numDisc = 1, numCont = 1)
      worlds.fillZeroMsg(msgs, Offsets())
      msgs.disc(0).msg(0) should be(0.0)
      msgs.disc(0).msg(1) should be(0.0)
      msgs.cont(0).mean should be(0.0)
    }

    "copy marginals into messages" in {
      val worlds = World.Values(Bools, Doubles)
      val msgs = new Msg(numDisc = 1, numCont = 1)
      val marginals = worlds.Marginals(Map(false -> 0.2, true -> 0.8), 0.5)
      worlds.copyMarginals(marginals, msgs, Offsets())
      msgs.disc(0).msg(0) should be(0.2)
      msgs.disc(0).msg(1) should be(0.8)
      msgs.cont(0).mean should be(0.5)
    }

    "create one and zero values" in {
      val worlds = World.Values(Bools, Doubles)
      worlds.zero should be(World(false, 0.0))
      worlds.one should be(World(true, 1.0))
    }

    "create a constant" in {
      val worlds = World.Values(Bools, Doubles)
      val const = worlds.Const(World(true,0.5))
      const.eval2() should be (World(true,0.5))
    }

    "create a constant using toTerm" in {
      implicit val worlds = World.Values(Bools, Doubles)
      val const = World(true,0.5).toConst
      const.rain.eval() should be (true)
    }

    "create a static variable" in {
      val worlds = World.Values(Bools, Doubles)
      val x = worlds.Var
      val y = x.rain
      x.eval(World(true,0.5)) should be (World(true,0.5))
      y.eval(World(true,0.5)) should be (true)
    }

    "create nested domains" in {
      import scala.language.existentials //todo: why is this necessary? General problem with sequence domains?
      @domain case class Params(weights:IndexedSeq[Double])
      val params = Params.Values(seqs(Doubles,2))
      val x = params.variable("x")
      val term = x.weights(1)
      term.eval(Params(IndexedSeq(1.0,2.0))) should be (2.0)
    }

    "work with classes defined elsewhere" in {
      import ml.wolfe.FactorieVector
      @domain case class Wrapped(vector:FactorieVector)
      val X = Wrapped.Values(Vectors(2))
      val x = X.variable("x")
      x.eval(Wrapped(vector(1,2))).vector should equal (vector(1,2))
    }

    "return all case class values as iterable" in {
      @domain case class DiscWorld(rain:Boolean, sprinkler:Boolean)
      val worlds = DiscWorld.Values(Bools,Bools)
      val result = worlds.toSet
      result should be (Set(DiscWorld(false,false),DiscWorld(false,true),DiscWorld(true,false),DiscWorld(true,true)))
    }

    "Work with nested case classes" in {
      @domain case class A(value:Int)
      @domain case class B(a:A)
      val bs = B.Values(A.Values(Discretes(1,2)))
      bs.toSet should be (Set(B(A(1)),B(A(2))))
    }

    "Create terms using a term constructor" in {
      val worlds = World.Values(Bools, Doubles)
      val b = Bools.Var
      val d = Doubles.Var
      val term = worlds.Term(b,d)
      term.eval2(true,0.5) should be (World(true,0.5))

    }


  }

}
