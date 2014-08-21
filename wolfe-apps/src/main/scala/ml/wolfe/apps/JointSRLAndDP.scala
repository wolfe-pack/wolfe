package ml.wolfe.apps

import ml.wolfe.FactorGraph.Edge
import ml.wolfe.Wolfe

/**
 * @author Sebastian Riedel
 */
object JointSRLAndDP extends App {

  import Wolfe._
  import ml.wolfe.macros.OptimizedOperators._

  val labels = List("A", "P")
  val root   = "Root"

  type Edge = (Int, Int)
  type Labelled = (Edge, String)

  case class Y(args: Pred[Edge], roles: Pred[Labelled], deps: Pred[Edge])
  case class X(words: Seq[String], tags: Seq[String]) {
    def n = words.size
    def candidateArgs = for (h <- 0 until n; m <- 0 until n) yield (h, m)
    def candidateDeps = for (h <- 0 until n; m <- 1 until n; if h != m) yield (h, m)
    def candidateRoles = for (edge <- candidateArgs; label <- labels) yield edge -> label
  }

  def example: (X, Y) = {
    val words = Seq(root, "Bob", "killed", "Anna")
    val tags = Seq(root, "NNP", "VBD", "NNP")
    val deps = Map((0, 2) -> true, (2, 1) -> true, (2, 3) -> true) withDefaultValue false
    val args = Map((2, 1) -> true, (2, 3) -> true) withDefaultValue false
    val roles = Map(((2, 1), "A") -> true, ((2, 3), "P") -> true) withDefaultValue false
    (X(words, tags), Y(args, roles, deps))
  }

  def argLocal(x: X)(y: Y)(h: Int, m: Int) =
    oneHot('ww ->(x.words(h), x.words(m), y.args(h, m)))

  def depLocal(x: X)(y: Y)(h: Int, m: Int) =
    oneHot('ww ->(x.words(h), x.words(m), y.deps(h, m)))


  def feats(x: X)(y: Y) =
    sum(x.candidateArgs) { case (h, m) => argLocal(x)(y)(h, m) } +
    sum(x.candidateDeps) { case (h, m) => depLocal(x)(y)(h, m) } +
    sum(x.candidateArgs) { case (h, m) => oneHot('nand, I(y.args(h, m) && y.deps(h, m))) }

  def model(w: Vector)(x: X)(y: Y) =
    feats(x)(y) dot w

  def localLoss(x: X, gold: Y)(w: Vector) = 0.0

  def loss(data: Seq[(X, Y)])(w: Vector) =
    sum(data) { case (x, y) => localLoss(x, y)(w) }

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
