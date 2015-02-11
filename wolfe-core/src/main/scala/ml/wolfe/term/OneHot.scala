package ml.wolfe.term

import cc.factorie.la.SingletonTensor1
import ml.wolfe.Index

///**
// * @author riedel
// */
//class OneHot(val index:IntTerm,val value:DoubleTerm)(val domain:VectorDom) extends Composed[VectorDom] {
//  val arguments = IndexedSeq(index,value)
//  def composer() = new Evaluator {
//    def eval(inputs: Array[Setting], output: Setting) = {
//      val index = inputs(0).disc(0)
//      val value = inputs(1).cont(0)
//      output.vect(0) = new SingletonTensor1(domain.dim,index,value)
//    }
//  }
//  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
//    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
//      ???
//    }
//    def withRespectTo = wrt
//  }
//}

class Indexed[T<:Term[Dom]](val value:T)(val index:Index) extends Composed[DiscreteDom[Int]] {
  def arguments = ???

  def composer() = ???

  def differentiator(wrt: Seq[Var[Dom]]) = ???

  val domain: DiscreteDom[Int] = ???
}