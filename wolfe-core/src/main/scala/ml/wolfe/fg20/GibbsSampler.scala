package ml.wolfe.fg20

import ml.wolfe.{MoreArrayOps, FactorieVector}
import MoreArrayOps._
import cc.factorie.maths._

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
abstract class GibbsSampler(val problem: Problem)(implicit random:Random = new Random(0)) extends FG with NodeContentFG {

  class NodeContent
  final class DiscNodeContent(size: Int) extends NodeContent {
    val probs = Array.ofDim[Double](size)
  }
  final class ContNodeContent() extends NodeContent

  def inferMarginals(burnIn: Int, samples: Int, weights: FactorieVector) = {
    for (sample <- 0 until samples) {
      for (n <- discNodes; if !n.observed) {
        sampleDiscNode(weights, n)
      }
    }
    ???
  }

  def sampleDiscNode(weights: FactorieVector, n: DiscNode) {
    fill(n.content.probs, 0.0)
    for (i <- 0 until n.variable.dom.size) {
      n.setting = i
      for (e <- n.edges)
        n.content.probs(i) += e.factor.pot.score(e.factor, weights)
    }
    expNormalize(n.content.probs)
    n.setting = nextDiscrete(n.content.probs)
  }
}
