package ml.wolfe.term

import ml.wolfe.term.TermImplicits._

import scala.util.Random

/**
 * @author riedel
 */
object LearningObjective {

  implicit val random = new Random(0)

  def perceptron[X <: Dom, Y <: Dom, W <: Dom](data:SeqTerm[X x Y])
                                              (space: Y)
                                              (model: W#Term => X#Term => Y#Term => DoubleTerm)
                                              (w:W#Term):DoubleTerm = {

    //how can search space be made dependent on x?
    shuffled(data) { i => max(space) {l => model(w)(i._1)(l)} - model(w)(i._1)(i._2)}

  }

}
