package ml.wolfe.fg20

import ml.wolfe._

import scala.annotation.tailrec

// ----------- Factor Graph ---------------


class MyImplies(premise: DiscVar[Boolean], consequent: DiscVar[Boolean]) extends BP.Pot {

  def discVars = Array(premise, consequent)
  def valueInBPFG(factor: BPFG#Factor, weights: FactorieVector) = {
    val p = factor.discEdges(0)
    val c = factor.discEdges(1)
    if (!premise.dom(p.node.content.setting) || consequent.dom(c.node.content.setting)) 0.0 else Double.NegativeInfinity
  }
  def maxMarginalExpectationsAndObjective(factor: BPFG#Factor, dstExpectations: FactorieVector) = ???
  def discMaxMarginalF2N(edge: BPFG#DiscEdge, weights: FactorieVector) = ???
  def contMaxMarginalF2N(edge: BPFG#ContEdge, weights: FactorieVector) = ???
}


object BP {
  trait Pot extends DiscPotential {
    def valueInBPFG(factor: BPFG#Factor, weights: FactorieVector): Double
    def discMaxMarginalF2N(edge: BPFG#DiscEdge, weights: FactorieVector)
    def contMaxMarginalF2N(edge: BPFG#ContEdge, weights: FactorieVector)
    def maxMarginalExpectationsAndObjective(factor: BPFG#Factor, dstExpectations: FactorieVector): Double
  }
}


trait BPFG extends FG {

  class DiscNodeContent(var setting: Int = 0,
                        var belief: Array[Double])

  class ContNodeContent(var setting: Double = 0.0,
                        var mean: Double = 0.0,
                        var dev: Double = 0.0)
  class FactorContent()
  class DiscMsgs(size: Int) {
    val f2n     = Array.ofDim[Double](size)
    val n2f     = Array.ofDim[Double](size)
    val f2nLast = Array.ofDim[Double](size)
  }
  class ContMsgs(var mean: Double, var dev: Double)

  def createDiscMsgs(variable: DiscVar[Any]) = new DiscMsgs(variable.dom.size)
  def createDiscNodeContent(variable: DiscVar[Any]) = new DiscNodeContent(0, Array.ofDim[Double](variable.dom.size))
  def createContNodeContent(contVar: ContVar) = new ContNodeContent()
  def createContMsgs(contVar: ContVar) = new ContMsgs(0.0, 0.0)

  def createFactorContent(pot: Pot) = new FactorContent

}

trait EdgePropagation extends FG with Scheduling {

  lazy val scheduled = MPSchedulerImpl.schedule()

  def propagate(maxIterations: Int, weights: FactorieVector): Unit = {
    var iteration = 0
    while (iteration < maxIterations) {
      for (directedEdge <- scheduled) directedEdge.direction match {
        case EdgeDirection.N2F =>
          updateN2F(directedEdge.edge, weights)

        case EdgeDirection.F2N =>
          updateF2N(directedEdge.edge, weights)
      }
      iteration += 1
    }
  }

  def updateF2N(edge: Edge, weights: FactorieVector)
  def updateN2F(edge: Edge, weights: FactorieVector)

}


// ------------- Inference ---------------
class MaxProduct(val problem: Problem) extends BPFG with EdgePropagation {

  type Pot = BP.Pot

  def acceptPotential = { case pot: BP.Pot => pot }

  def updateF2N(edge: Edge, weights: FactorieVector) = {
    edge match {
      case d: DiscEdge => edge.factor.pot.discMaxMarginalF2N(d, weights)
      case c: ContEdge => edge.factor.pot.contMaxMarginalF2N(c, weights)
    }
  }

  def updateN2F(edge: Edge, weights: FactorieVector) = {
    edge match {
      case d: DiscEdge =>
        for (i <- d.msgs.n2f.indices)
          d.msgs.n2f(i) = { for (e <- d.node.edges if e != edge) yield e.msgs.f2n(i) }.sum
      case _ =>
    }
  }

  def inferMAP(weights: FactorieVector): MAPResult = {
    propagate(10, weights)
    val gradient = new SparseVector(1000)
    val score = factors.view.map(f => f.pot.maxMarginalExpectationsAndObjective(f, gradient)).sum
    val discState = problem.discVars.map(v => v -> v.dom(discNodes(v).content.setting)).toMap[Var[Any], Any]
    val contState = problem.contVars.map(v => v -> contNodes(v).content.setting)
    MAPResult(new State(discState ++ contState), score, gradient)
  }

}

trait Scheduling {

  this: FG =>

  object EdgeDirection extends Enumeration {
    type EdgeDirection = Value
    val N2F, F2N = Value
  }

  import EdgeDirection._

  case class DirectedEdge(edge: Edge, direction: EdgeDirection) {
    def f = edge.factor
    def n = edge.node
    def swap = DirectedEdge(edge, if (direction == N2F) F2N else N2F)
    override def toString = if (direction == N2F) s"n[$n] -> f[$f]" else s"f[$f] -> n[$n]"
  }

  /**
   * A scheduler provides a canonical ordering of edges such that it resembles the message ordering of forward-backward.
   */
  trait MPScheduler {

    /**
     * @param node root node
     * @return correct message ordering for forward pass
     */
    def scheduleForward(node: Node): Seq[DirectedEdge]

    def schedule(node: Node): Seq[DirectedEdge] = {
      val forward = scheduleForward(node)
      val backward = forward.reverse.map(_.swap)
      forward ++ backward
    }

    /**
     * Runs scheduler on all disconnected components of the graph
     * @return schedule for forward-backward over all disconnected components of the graph
     */
    def schedule(): Seq[DirectedEdge] = {
      val forward = scheduleForward()
      val backward = forward.reverse.map(_.swap)
      forward ++ backward
    }

    def scheduleForward(): Seq[DirectedEdge] = {
      @tailrec
      def scheduleAcc(nodes: Seq[Node], done: Set[Node], acc: Seq[DirectedEdge]): Seq[DirectedEdge] = nodes match {
        case Nil => acc
        case head :: tail =>
          if (done.contains(head)) scheduleAcc(tail, done, acc)
          else {
            val edges = scheduleForward(head)
            scheduleAcc(tail, done ++ edges.map(_.edge.node), acc ++ edges)
          }
      }

      scheduleAcc(nodes.toList, Set(), Seq())
    }

    def schedule(factor: Factor): Seq[DirectedEdge] = schedule(factor.edges.head)

    def schedule(edge: Edge): Seq[DirectedEdge] = schedule(edge.node)

    /**
     * Returns a schedule based on the order edges were added to the factor graph
     * @return schedule
     */
    def canonicalSchedule(): Seq[DirectedEdge] = {
      @tailrec
      def canonicalSchedule(edges: List[Edge], done: Set[Edge], acc: Seq[DirectedEdge]): Seq[DirectedEdge] = edges match {
        case Nil => acc
        case e :: es =>
          if (!done.contains(e)) {
            val siblings = e.factor.edges.filter(_ != e)
            val f2n = DirectedEdge(e, EdgeDirection.F2N)
            val n2fs = siblings.map(DirectedEdge(_, EdgeDirection.N2F))
            canonicalSchedule(es, done ++ siblings + e, acc ++ n2fs ++ List(f2n))
          }
          else canonicalSchedule(es, done, acc)
      }
      canonicalSchedule(edges.toList, Set(), Nil)
    }
  }

  object MPSchedulerImpl extends MPScheduler {
    /**
     * @param node The node which will become the root node
     * @return correct ordering for messaging pass for given direction (excluding the staring edge)
     */
    def scheduleForward(node: Node): Seq[DirectedEdge] = {
      @tailrec
      def scheduleAcc(todo: List[DirectedEdge], done: Set[Edge], acc: Seq[DirectedEdge]): Seq[DirectedEdge] =
        todo match {
          case Nil => acc
          case head :: tail =>
            if (done.contains(head.edge)) scheduleAcc(tail, done, acc)
            else {
              val parents = head.direction match {
                case EdgeDirection.N2F =>
                  head.n.edgeList.
                  filterNot(e => e == head.edge || todo.view.map(_.edge).contains(e)).
                  map(DirectedEdge(_, EdgeDirection.F2N))
                case EdgeDirection.F2N =>
                  head.f.edges.
                  filterNot(e => e == head.edge || todo.view.map(_.edge).contains(e)).
                  map(DirectedEdge(_, EdgeDirection.N2F))
              }

              scheduleAcc(tail ++ parents, done + head.edge, parents ++ acc)
            }
        }
      val firstEdges = node.edgeList.map(DirectedEdge(_, EdgeDirection.F2N)).toList
      scheduleAcc(firstEdges, Set(), firstEdges)
    }
  }

}


