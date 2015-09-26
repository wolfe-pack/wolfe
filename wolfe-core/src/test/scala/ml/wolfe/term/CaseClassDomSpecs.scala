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
      val marginals = worlds.toMarginals(msgs, Offsets())
      marginals should be(worlds.Marginals(Map(false -> 0.8, true -> 0.2), 0.5))
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
      val const = worlds.Const(World(true, 0.5))
      const.eval() should be(World(true, 0.5))
    }

    "create a constant using toTerm" in {
      implicit val worlds = World.Values(Bools, Doubles)
      val const = World(true, 0.5).toConst
      const.rain.eval() should be(true)
    }

    "create a static variable" in {
      val worlds = World.Values(Bools, Doubles)
      val x = worlds.Var
      val y = x.rain
      x.eval(x := World(true, 0.5)) should be(World(true, 0.5))
      y.eval(x := World(true, 0.5)) should be(true)
    }

    "create nested domains" in {
      import scala.language.existentials
      //todo: why is this necessary? General problem with sequence domains?
      @domain case class Params(weights: IndexedSeq[Double])
      val params = Params.Values(Seqs(Doubles, 2))
      val x = params.Var
      val term = x.weights(1)
      term.eval(x := Params(IndexedSeq(1.0, 2.0))) should be(2.0)
    }

    "work with classes defined elsewhere" in {
      import ml.wolfe.Vect
      @domain case class Wrapped(vector: Vect)
      val X = Wrapped.Values(Vectors(2))
      val x = X.Var
      x.eval(x := Wrapped(vector(1, 2))).vector should equal(vector(1, 2))
    }

    "return all case class values as iterable" in {
      @domain case class DiscWorld(rain: Boolean, sprinkler: Boolean)
      val worlds = DiscWorld.Values(Bools, Bools)
      val result = worlds.toSet
      result should be(Set(DiscWorld(false, false), DiscWorld(false, true), DiscWorld(true, false), DiscWorld(true, true)))
    }

    "Work with nested case classes" in {
      @domain case class A(value: Int)
      @domain case class B(a: A)
      val bs = B.Values(A.Values(Discretes(1, 2)))
      bs.toSet should be(Set(B(A(1)), B(A(2))))
    }

    "Create terms using a term constructor" in {
      val Worlds = World.Values(Bools, Doubles)
      val b = Bools.Var
      val d = Doubles.Var
      val term = Worlds.Term(b, d)
      term.eval(b := true, d := 0.5) should be(World(true, 0.5))

    }

    "Create a variable that has accessor terms" in {
      val Worlds = World.Values(Bools, Doubles)
      val x = Worlds.Var
      (x.prob | x <<  World(true, 0.3)).eval() should be (0.3)
    }


    "Create a copy method of case class terms" in {
      val Worlds = World.Values(Bools, Doubles)
      val b = Bools.Var
      val d = Doubles.Var
      val term = Worlds.Term(b,d)
      val copied = term.copy(IndexedSeq(Bools.Const(true),d))
      copied.eval(d := 0.5) should be (World(true,0.5))
    }

    "create different indices for the same values when used with object semantics" in {
      val Worlds = World.Objects(Bools,Doubles)
      val s1 = Worlds.toSetting(World(true, 0.5))
      val s2 = Worlds.toSetting(World(true, 0.5))

      Worlds.indexOfSetting(s1) should not be Worlds.indexOfSetting(s2)
    }

    "create the same indices for the same values when used with value semantics" in {
      @domain case class Example(x:Boolean, y:Boolean)
      val Examples = Example.Values(Bools,Bools)
      val s1 = Examples.toSetting(Example(true, false))
      val s2 = Examples.toSetting(Example(true, false))

      Examples.indexOfSetting(s1) should be (Examples.indexOfSetting(s2))
    }

    "should support generics" in {
      @domain case class Stack[C](store:IndexedSeq[C])

      val Stacks = Stack.Values[Double](Seqs(Doubles,5))
      Stacks.store.elementDom should be (Doubles)

      val test:Stack.Dom[Double] = Stacks

      val v:Stack.Term[Double] = Stacks.Variable("x")

//      v.store

      def example[C](stack:Stack.Term[C]) = ??? //stack.store(0)

//      def head[C,D <: TypedDom[IndexedSeq[C]]](stack:Stack.Dom[C,D]#Term) = stack.store
//
//
//      val typed:Stack.Dom[Double,_] = Stacks
//      val term:Term[Stack.Dom[_,_]] = Stacks.Variable("x")
      //def (stack: Stack.Term[C,]
      //trait Test2[type A]

    }

  }


}
