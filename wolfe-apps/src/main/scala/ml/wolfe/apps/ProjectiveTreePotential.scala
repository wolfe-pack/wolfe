package ml.wolfe.apps

import ml.wolfe.fg20._

/**
 * A potential that tests whether a set of edges (represented as assignment set of boolean variables)
 * is a projective dependency tree. To provide edge scores use incoming messages.
 * @author Sebastian Riedel
 */
class ProjectiveTreePotential(val slen: Int, val edgeVars: Map[(Int, Int), DiscVar[Any]]) extends DiscPotential {

  import ml.wolfe.apps.ProjectiveTreePotential._

  private val distinctHeads = edgeVars.keys.map(_._1).toSeq.distinct.sorted

  val discVars = (for (h <- 0 until slen; m <- 1 until slen) yield edgeVars(h, m)).toArray

  final def scoreEdge(msg: Msgs, h: Int, m: Int) =
    msg.disc(indexOf(slen, 0, 1)).msg(0) - msg.disc(indexOf(slen, 0, 1)).msg(1)

  class Proc extends ArgmaxProcessor {
    def argmax(obs: PartialSetting, incoming: Msgs, result: Setting) = {
      val s01 = scoreEdge(incoming, 0, 1)
      0.0
    }
    def score(setting: Setting) = {
      //check if tree
      0.0
    }
  }

  def scorer() = new Proc

}

object ProjectiveTreePotential {
  def indexOf(slen:Int, h: Int, m: Int) = h * slen + m
}

