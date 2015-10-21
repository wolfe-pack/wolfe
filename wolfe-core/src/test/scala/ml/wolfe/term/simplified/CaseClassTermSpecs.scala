package ml.wolfe.term.simplified

import ml.wolfe.WolfeSpec
import org.scalautils.Good

/**
 * @author riedel
 */
class CaseClassTermSpecs extends WolfeSpec {

  import BaseEval._
  import Wolfe._

  "A term macro" should {
    "provide getters on terms" in {
      @term case class Stack[C](store: Seq[C], strength: Seq[Double])
      val v = Var[Stack[Double]]
      eval(v := Stack(Seq(1.0, 2.0), Seq(2.0, 3.0)))(v.store) should be(Good(Seq(1.0, 2.0)))
    }
    "provide a term constructor" in {
      @term case class Stack[C](store: Seq[C], active: Double)
      val v = Var[Double]
      val t = Stack.Term(Seq(1.0),v)
      eval(v := 3.0)(t) should be (Good(Stack(Seq(1.0),3.0)))
    }


    "provide a term unapply method" in {
      @term case class Stack[C](store: Seq[C], active: Double)
      val r = Constant(Stack(Seq(1.0),2.0)) match {
        case Stack.Term(store,_) => store
      }
      eval()(r) should be (Good(Seq(1.0)))
    }

    "provide a domain constructor" in {
      @term case class Test[C](a:C, b:Int)
      val Tests = Test.Values(RangeDom(0 until 2), RangeDom(2 until 3))
      Tests shouldBe a [ProductDom[_]]
      Tests.doms(0) should be (RangeDom(0 until 2))
      Tests.doms(1) should be (RangeDom(2 until 3))
    }

//    "work with plain classes" in {
//      @term class Param(k:Int) {
//        val List(w,i) = List(1.0, 2.0)
//      }
//
//    }

  }
}
