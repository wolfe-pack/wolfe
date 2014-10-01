package ml.wolfe.apps

import ml.wolfe.{BruteForceOperators, Wolfe}

/**
 * @author Sebastian Riedel
 */
object MultiLayerPerceptron {

  import Wolfe._
  import ml.wolfe.macros.OptimizedOperators._
  import math._

  type MLP = Seq[Seq[Double]]

  case class Instance(mlp:MLP,label:Boolean)

  def space: Iterable[MLP] = ???

  def layerPredicate(w: Vector)(l: Int, mlp: MLP) = {
    forall(0 until mlp(l).length) { i =>
      mlp(l)(i) == tanh(sum(0 until mlp(l - 1).length) { j => w((l, i, j)) * mlp(l - 1)(j) })
    }
  }

  def layer(w: Vector)(l: Int, mlp: MLP) = {
    sum(0 until mlp(l).length) { i =>
      logI(mlp(l)(i) == tanh(sum(0 until mlp(l - 1).length) { j => w((l, i, j)) * mlp(l - 1)(j) }))
    }
  }

  def mlp(w: Vector)(m: MLP) = sum(0 until m.length) { l => layer(w)(l, m) }

  def propagate(w: Vector)(x:Seq[Double]) = BruteForceOperators.argmax(space where (m => m(0) == x)) { mlp(w) }

  def f(i:Instance) = sum(0 until i.mlp.last.length) { j => oneHot((i.label,j))}

  def classifier(w:Vector)(i:Instance) = mlp(w)(i.mlp) + (f(i) dot w)




}
