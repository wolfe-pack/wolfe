package ml.wolfe.apps

import cc.factorie.optimize.{AdaGrad, OnlineTrainer}
import ml.wolfe.nlp.DependencyTree
import ml.wolfe.{BeliefPropagation, Wolfe}
import ml.wolfe.fg.TreePotential

/**
 * Created by narad on 9/16/14.
 */
object GraphBasedDependencyParser extends App {

  import Wolfe._
  import ml.wolfe.macros.OptimizedOperators._
  import TreePotential._
  import ml.wolfe.nlp.io.CoNLLReader

  val root   = "Root"

  type Edge = (Int, Int)

  case class Y(deps: Pred[Edge])
  case class X(words: Seq[String], tags: Seq[String])

  def candidateDeps(x: X) = { val n = x.words.size; for (h <- 0 until n; m <- 1 until n; if h != m) yield (h, m) }

  def space(x: X) = all(Y) {
    preds(candidateDeps(x))
  }

  def depLocal(x: X)(y: Y)(e: (Int, Int)) =
    oneHot('d_ww ->(x.words(e._1), x.words(e._2), y.deps(e)))

  def feats(x: X)(y: Y) = {
    sum(candidateDeps(x)) { e => depLocal(x)(y)(e) }
  }

  @LogZByInference(BeliefPropagation.sumProductPow(3))
  def model(w: Vector)(x: X)(y: Y) =
    treeConstraint(y.deps) + (feats(x)(y) dot w)

  def localLoss(x: X, gold: Y)(w: Vector) =
    logZ(space(x)) { model(w)(x) }

  @OptimizeByLearning(new OnlineTrainer(_, new AdaGrad(), 20, -1))
  def loss(data: Seq[(X, Y)])(w: Vector) =
    sum(data) { i => localLoss(i._1, i._2)(w) } // todo: gradient calculator can't deal with "case (x,y)"

  val example: (X, Y) = {
    val words = Seq(root, "Bob", "killed", "Ann")
    val tags = Seq(root, "NNP", "VBD", "NNP")
    val deps = Map((0, 2) -> true, (2, 1) -> true, (2, 3) -> true) withDefaultValue false
     (X(words, tags), Y(deps))
  }

  def treeToInstance(tree: DependencyTree): (X, Y) = {
    val words = Seq(root) ++ tree.tokens.map(_.word)
    val tags = Seq(root) ++ tree.tokens.map(_.posTag)
    val deps = tree.arcs.map {a => (a._1, a._2) -> true }.toMap withDefaultValue false
      (X(words, tags), Y(deps))
  }

  val train = new CoNLLReader(args(0)).map(t => treeToInstance(t.syntax.dependencies))

  val w = argmin(vectors) { loss(train) }

  println(w)

  val test = new CoNLLReader(args(1)).map(t => treeToInstance(t.syntax.dependencies))

  test.foreach { t =>
//    val exp = expect (space(t._1)) (model(w)(t._1)) (x => oneHot("hello"))
//    println(exp)
    println
  }
}
