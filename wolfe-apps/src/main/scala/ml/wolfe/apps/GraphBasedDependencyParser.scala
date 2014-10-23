package ml.wolfe.apps

import cc.factorie.optimize.{LBFGS, AdaGrad, OnlineTrainer}
import ml.wolfe.nlp.{CharOffsets, Token, DependencyTree}
import ml.wolfe.{BeliefPropagation, Wolfe}
import ml.wolfe.fg.TreePotential

/**
 * Created by narad on 9/16/14.
 */
object GraphBasedDependencyParser extends App {

  import Wolfe._
  import ml.wolfe.macros.OptimizedOperators._
  import TreePotential._
  import ml.wolfe.D3Implicits._
  import ml.wolfe.nlp.io.CoNLLReader

  val root = "Root"

  type Edge = (Int, Int)

  case class Y(deps: Pred[Edge])
  case class X(words: Seq[String], tags: Seq[String])

  def candidateDeps(x: X) = { val n = x.words.size; for (h <- 0 until n; m <- 1 until n; if h != m) yield (h, m) }

  def space(x: X) = all(Y) {
    preds(candidateDeps(x))
  }

  def depLocal(x: X)(y: Y)(e: (Int, Int)) = {
    val ff = oneHot('d_ww ->(x.words(e._1), x.words(e._2)), I(y.deps(e)))
    ff
  }

  def feats(x: X)(y: Y) = {
    sum(candidateDeps(x)) { e =>
      depLocal(x)(y)(e)
    }
  }

  @OutputFactorGraph
  @LogZByInference(BeliefPropagation.sumProductPow(3))
  def model(w: Vector)(x: X)(y: Y) = (feats(x)(y) dot w)

  def localLoss(x: X, gold: Y)(w: Vector) =
    logZ(space(x)) { model(w)(x) } - model(w)(x)(gold)

  @OptimizeByLearning(new OnlineTrainer(_, new AdaGrad, 20, -1))
  def loss(data: Seq[(X, Y)])(w: Vector) =
    sum(data) { i =>
      localLoss(i._1, i._2)(w)
    } // todo: LBFGS gradient calculator can't deal with "case (x,y)"

  def treeToInstance(tree: DependencyTree): (X, Y) = {
    val words = Seq(root) ++ tree.tokens.map(_.word)
    val tags = Seq(root) ++ tree.tokens.map(_.posTag)
    val deps = tree.arcs.map {a => (a._2, a._1) -> true }.toMap withDefaultValue false
      (X(words, tags), Y(deps))
  }

  def instanceToTree(x: X): DependencyTree = {
    val tokens = x.words.zip(x.tags).zipWithIndex.map { case((tw,tt),i) => Token(word = tw, offsets = CharOffsets(i,i), posTag = tt)}.tail
    val exp = expect(space(x))(model(w)(x))(y => sum(candidateDeps(x)) { e => oneHot(e, I(y.deps(e)))})
    val arcs = candidateDeps(x).flatMap { case(i,j) =>
//      val exp = expect (space(x)) (model(w)(x)) (s => oneHot(0 -> 1, I(s.deps(i,j)))).get((0,1)).get
      exp(i -> j) match {
        case p if p > 0.5 => Some((j, i, null))
        case _ => None
      }
    }
    DependencyTree(tokens, arcs)
  }

  val train = new CoNLLReader(args(0)).view.map(t => treeToInstance(t.syntax.dependencies))
  val test = new CoNLLReader(args(1)).view.map(t => treeToInstance(t.syntax.dependencies))

  val w = argmin(vectors) { loss(train) }

  println("Testing...")
  var correctArcs = 0
  var predictedArcs = 0
  test.foreach { case(x,y) =>
    val tree = instanceToTree(x)
    val gold = DependencyTree(tree.tokens, y.deps.map(d => (d._1._2, d._1._1, null)).toSeq)
    val correct = tree.arcs.filter(gold.arcs.contains(_)).size
    correctArcs += correct
    predictedArcs += tree.size
  }
  println("Parse Accuracy: %.2f".format(100.0 * correctArcs / predictedArcs))
}






// "/Users/narad/Documents/work/data/conll/2009/english/evaluation.txt"

//  saveD3Graph(FactorGraphBuffer.get, "/Users/narad/Documents/work/projects/factorgraph.html")

// /Users/narad/Documents/work/data/conll/2009/english/evaluation.txt