package ml.wolfe.examples

import ml.wolfe.Wolfe
import ml.wolfe.macros.OptimizedOperators
import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import ml.wolfe.util.Evaluator

/**
 * @author Sebastian Riedel
 */
object GateExample {

  import Wolfe._
  import OptimizedOperators._

  case class XY(in1: Boolean, in2: Boolean, out: Boolean)

  def f(d:XY) = oneHot('in1, I(d.in1 && d.out)) + oneHot('in2, I(d.in2 && d.out)) + oneHot('bias, I(d.out))

  def s(w: Vector)(d: XY) = w dot f(d)

  def q(obs: XY)(d: XY) = d.in1 == obs.in1 && d.in2 == obs.in2

  def h(w: Vector)(obs: XY) = argmax { over(all(XY)) of s(w) st q(obs) }

  @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 10))
  def l(data: Seq[XY])(w: Vector) = sum { over(data) of (d => s(w)(h(w)(d)) - s(w)(d)) }

  val andData = Seq(
    XY(true, true, true),
    XY(false, true, false),
    XY(true, false, false),
    XY(false, false, false))

  def main(args: Array[String]) {

    val w = argmin { over(vectors) of l(andData) }

    println(w)

    val guess = h(w)(XY(true,true,true))

    println(guess)
    println(Evaluator.evaluate(andData,andData.map(h(w)))(_.out))
  }




}
