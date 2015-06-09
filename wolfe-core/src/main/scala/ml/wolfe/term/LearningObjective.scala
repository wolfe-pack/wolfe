package ml.wolfe.term

import ml.wolfe.term.TermImplicits._

import scala.util.Random

/**
 * @author riedel
 */
object LearningObjective {

  implicit val random = new Random(0)

  //want to write perceptron(train)(Labels)(x => y => model(w)(x)(y)

  //  def genericHammingDistance(D: Dom)(t1: Term[D.type], t2: Term[D.type]) : DoubleTerm = {
  //    D match {
  //      case s: VarSeqDom[_] =>
  //        val s1 = s.own(t1.asInstanceOf[TypedTerm[s.Value]])
  //        val s2 = s.own(t2.asInstanceOf[TypedTerm[s.Value]])
  //        sum(0 until s1.length) { i =>
  //          genericHammingDistance(s.elementDom)(s1(i), s2(i))
  //        }
  //      case s:GenericDiscreteDom[_] => I(! (s.own(t1.asInstanceOf[TypedTerm[s.Value]]) === s.own(t2.asInstanceOf[TypedTerm[s.Value]])))
  //      case _ => ???
  //    }
  //  }

  def perceptron[X <: Dom, Y <: Dom, W <: Dom](data: SeqTerm[X x Y])
                                              (space: Y)
                                              (model: X#Term => Y#Term => DoubleTerm)
                                              (implicit random: Random): DoubleTerm = {

    //shuffled(data) { i => max(space) {model(i._1)} - model(i._1)(i._2)}
    shuffled(data) { i => model(i._1)(i._2) - max(space) {
      model(i._1)
    }
    }
  }

}
