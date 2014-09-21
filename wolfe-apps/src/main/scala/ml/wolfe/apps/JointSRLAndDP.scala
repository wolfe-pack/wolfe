package ml.wolfe.apps

import cc.factorie.optimize.{AdaGrad, AveragedPerceptron, OnlineTrainer}
import ml.wolfe.{BeliefPropagation, FactorGraph, Learn, Wolfe}
import ml.wolfe.fg.TreePotential

/**
 * @author Sebastian Riedel
 */
object JointSRLAndDP extends App {

  import Wolfe._
  import ml.wolfe.macros.OptimizedOperators._
  import TreePotential._

  val labels = List("A", "P")
  val root   = "Root"

  type Edge = (Int, Int)
  type Labelled = (Edge, String)

  case class Y(args: Pred[Edge], roles: Pred[Labelled], deps: Pred[Edge])
  case class X(words: Seq[String], tags: Seq[String])

  //todo: wolfe cannot deal with class member methods, that's why we put these methods outside of X
  def candidateArgs(x: X) = { val n = x.words.size; for (h <- 1 until n; m <- 1 until n) yield (h, m) }
  def candidateDeps(x: X) = { val n = x.words.size; for (h <- 0 until n; m <- 1 until n; if h != m) yield (h, m) }
  def candidateRoles(x: X) = for (edge <- candidateArgs(x); label <- labels) yield edge -> label
  def overlap(x: X) = { val n = x.words.size; for (h <- 1 until n; m <- 1 until n; if h != m) yield (h, m) }


  def space(x: X) = all(Y) {
    preds(candidateArgs(x)) x
    preds(candidateRoles(x)) x
    preds(candidateDeps(x))
  }

  def argLocal(x: X)(y: Y)(e: (Int, Int)) = {
    oneHot('a_ww ->(x.words(e._1), x.words(e._2), y.args(e)))
  }

  def depLocal(x: X)(y: Y)(e: (Int, Int)) =
    oneHot('d_ww ->(x.words(e._1), x.words(e._2), y.deps(e)))

  //todo: wolfe cannot deal with pattern matching functions
  //todo: wolfe cannot deal with conditions in sums
  def feats(x: X)(y: Y) = {
    sum(candidateArgs(x)) { e => argLocal(x)(y)(e) } +
    sum(candidateDeps(x)) { e => depLocal(x)(y)(e) } +
    sum(overlap(x)) { e => oneHot('nand, I(y.args(e) && y.deps(e))) }
  }

  @LogZByInference(BeliefPropagation.sumProductPow(3))
  def model(w: Vector)(x: X)(y: Y) =
    treeConstraint(y.deps) + (feats(x)(y) dot w)

  def localLoss(x: X, gold: Y)(w: Vector) =
    logZ(space(x)) { model(w)(x) } -
    logZ(space(x) where (y => y.args == gold.args)) { model(w)(x) }

  @OptimizeByLearning(new OnlineTrainer(_, new AdaGrad(), 20, -1))
  def loss(data: Seq[(X, Y)])(w: Vector) =
    sum(data) { i => localLoss(i._1, i._2)(w) } // todo: gradient calculator can't deal with "case (x,y)"

  val example: (X, Y) = {
    val words = Seq(root, "Bob", "killed", "Ann")
    val tags = Seq(root, "NNP", "VBD", "NNP")
    val deps = Map((0, 2) -> true, (2, 1) -> true, (2, 3) -> true) withDefaultValue false
    val args = Map((2, 1) -> true, (2, 3) -> true) withDefaultValue false
    val roles = Map(((2, 1), "A") -> true, ((2, 3), "P") -> true) withDefaultValue false
    (X(words, tags), Y(args, roles, deps))
  }

  val train = Seq(example)

  val w = argmin(vectors) { loss(train) }

  println(w)

  def query(y:Y) = oneHot(0->1,1.0)

//  val exp = expect (space(example._1)) (model(w)(example._1)) (query)



  //  case class Sentence(words: Seq[String], tags: Seq[String],
  //                      args: Pred[(Int, Int)],
  //                      roles: Pred[(Int, Int, Symbol)],
  //                      dp: Pred[(Int, Int)])
  //
  //  def argLocal(i: Int, j: Int)(s: Sentence) = {
  //    oneHot('ww ->(s.words(i), s.words(j), s.args(i, j)))
  //  }
  //
  //
  //  def feats(s: Sentence) =
  //    sum(s.args.keys) { case (i, j) => argLocal(i, j)(s) } +
  //    sum(s.args.keys) { case (i, j) => oneHot('nand, I(s.args(i, j) && s.dp(i, j))) }
  //
  //  def constraints(s: Sentence) =
  //    sum(s.args.keys) { case (i, j) => leq(s.args(i, j), labels map (s.roles(i, j, _))) } +
  //    tree(s.dp)
  //
  //  @Potential(leqPot(_, _))
  //  def leq(indicator: Boolean, others: Seq[Boolean]) =
  //    I(others.count(identity) <= 1 && indicator == others.exists(identity))
  //
  //  @Potential(treePot(_))
  //  def tree[T](graph: Map[(T, T), Boolean]) = Double.NegativeInfinity
  //
  //  def model(w: Vector)(s: Sentence) = {
  //    (feats(s) dot w) + constraints(s)
  //  }
  //
  //
  //
  //  def treePot(graph: Map[(Any, Any), Edge]) = new ml.wolfe.fg.Potential {}
  //
  //  def leqPot(indicator: Edge, others: Seq[Edge]) = new ml.wolfe.fg.Potential {
  //    /**
  //     * Calculate and update the marginal message from the factor of this potential to
  //     * the given edge.
  //     * @param edge the edge to pass the message on.
  //     */
  //    override def marginalF2N(edge: Edge) = {
  //      ???
  //    }
  //  }


}
