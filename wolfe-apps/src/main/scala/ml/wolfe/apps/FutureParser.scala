package ml.wolfe.apps

import cc.factorie.optimize.{LBFGS, BatchTrainer}
import ml.wolfe._
import ml.wolfe.Wolfe.Pred
import ml.wolfe.fg20.{GradientBasedOptimizer, Problem, VectVar}
import ml.wolfe.macros.OptimizedOperators._
import ml.wolfe.nlp.DependencyTree
import ml.wolfe.nlp.io.CoNLLReader

/**
 * Created by narad on 11/26/14.
 */
object FutureParser extends App {

  val train = new CoNLLReader(args(0))

  for (t <- train) println(t.toCoNLLString + "\n")

//  type X = DependencyTree

  val root = "Root"

  type Edge = (Int, Int)

  case class Y(deps: Pred[Edge])
  case class X(words: Seq[String], tags: Seq[String])

  def candidateDeps(x: X) = { val n = x.words.size; for (h <- 0 until n; m <- 1 until n; if h != m) yield (h, m) }

}

/*  def space(x: X) = all(Y) {
    preds(candidateDeps(x))
  }

  def depLocal(x: X)(y: Y)(e: (Int, Int)) = {
    val slen = x.words.size
    val distance = math.abs(e._1 - e._2)
    val dir = if (e._1 < e._2) "RIGHT" else "LEFT"
    oneHot('d_bias -> "BIAS", I(y.deps(e))) +
    oneHot('d_ww ->(x.words(e._1), x.words(e._2)), I(y.deps(e)))
  }

  def feats(x: X)(y: Y) = {
    sum(candidateDeps(x)) { e =>
      depLocal(x)(y)(e)
    }
  }*/

//  def learn[X](data: Seq[(X, Double)], reg: Double = 0.1)(feat: X => FactorieVector): LinearRegressor[X] = {
//    val instances = data.toList.map({ case (x, y) => y -> feat(x) })
//    val maxDim = instances.iterator.map(_._2.dim1).max
//    val weightsVar = new VectVar(maxDim, "w")
//    val loss = for ((y, f) <- instances) yield new L2NormLoss(weightsVar, f, y)
//    val problem = Problem(new L2Regularizer(weightsVar, reg) :: loss)
//    val optimizer = new GradientBasedOptimizer(problem)
//    val result = optimizer.argmax(new BatchTrainer(_, new LBFGS(), 100))
//    new LinearRegressor(result.state(weightsVar), feat)



