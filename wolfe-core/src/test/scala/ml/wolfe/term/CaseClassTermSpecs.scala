package ml.wolfe.term

import ml.wolfe.term.WolfeSpec
import ml.wolfe.term._
import org.scalautils.Good

/**
 * @author riedel
 */
class CaseClassTermSpecs extends WolfeSpec {

  import BaseEval._
  import Wolfe._

  "A case class term macro" should {
    "provide getters on terms" in {
      @termdef case class Stack[C](store: Seq[C], strength: Seq[Double])
      val v = Var[Stack[Double]]
      eval(v := Stack(Seq(1.0, 2.0), Seq(2.0, 3.0)))(v.store) should be(Good(Seq(1.0, 2.0)))
    }
    "provide a term constructor" in {
      @termdef case class Stack[C](store: Seq[C], active: Double)
      val v = Var[Double]
      val t = Stack.Term(Seq(1.0),v)
      eval(v := 3.0)(t) should be (Good(Stack(Seq(1.0),3.0)))
    }


    "provide a term unapply method" in {
      @termdef case class Stack[C](store: Seq[C], active: Double)
      val r = Constant(Stack(Seq(1.0),2.0)) match {
        case Stack.Term(store,_) => store
      }
      eval()(r) should be (Good(Seq(1.0)))
    }

    "provide a domain constructor" in {
      @termdef case class Test[C](a:C, b:Int)
      val Tests = Test.Values(RangeDom(0 until 2), RangeDom(2 until 3))
      Tests shouldBe a [ProductDom[_]]
      Tests.doms(0) should be (RangeDom(0 until 2))
      Tests.doms(1) should be (RangeDom(2 until 3))
    }

  }
}
