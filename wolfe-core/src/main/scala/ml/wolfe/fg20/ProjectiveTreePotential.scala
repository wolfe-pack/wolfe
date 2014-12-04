package ml.wolfe.fg20

/**
 * A potential that tests whether a set of edges (represented as assignment set of boolean variables)
 * is a projective dependency tree. To provide edge scores use incoming messages.
 * @author Sebastian Riedel
 */
class ProjectiveTreePotential(val slen: Int,
                              val edges: GraphSearchSpace.Disc[Boolean],
                              val scores: GraphSearchSpace.Cont) extends Potential with SupportsArgmax {

  import ProjectiveTreePotential._

  //
  //  private val distinctHeads = edgeVars.keys.map(_._1).toSeq.distinct.sorted
  //
  val discVars = edges.discVars
  val contVars = scores.contVars
  def vectVars = Potential.emptyVectVars


  //
  //  final def scoreEdge(msg: Msgs, h: Int, m: Int) =
  //    msg.disc(indexOf(slen, 0, 1)).msg(0) - msg.disc(indexOf(slen, 0, 1)).msg(1)

  class Proc extends Argmaxer with Scorer {
    def argmax(obs: PartialSetting, incoming: Msgs, result: Setting, score: DoubleBuffer) = {
      //      val s01 = scoreEdge(incoming, 0, 1)
    }
    def score(setting: Setting) = {
      //check if tree
      0.0
    }
  }

  def scorer() = new Proc
  def argmaxer() = new Proc
}

object ProjectiveTreePotential {
  def indexOf(slen: Int, h: Int, m: Int) = h * slen + m
}

