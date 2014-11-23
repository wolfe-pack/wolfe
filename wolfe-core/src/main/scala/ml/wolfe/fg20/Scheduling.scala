package ml.wolfe.fg20

import scala.annotation.tailrec

/**
 * @author Sebastian Riedel
 */
trait Scheduling {

  this: FactorGraph =>

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

      scheduleAcc(nodes.filterNot(_.observed).toList, Set(), Seq())
    }

    def schedule(factor: Factor): Seq[DirectedEdge] = schedule(factor.edges.next())

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
                  filterNot(e => e.node.observed || e == head.edge || todo.view.map(_.edge).contains(e)).
                  map(DirectedEdge(_, EdgeDirection.F2N))
                case EdgeDirection.F2N =>
                  head.f.edges.
                  filterNot(e => e.node.observed || e == head.edge || todo.view.map(_.edge).contains(e)).
                  map(DirectedEdge(_, EdgeDirection.N2F)).toSeq
              }

              scheduleAcc(tail ++ parents, done + head.edge, parents ++ acc)
            }
        }
      val firstEdges = node.edgeList.iterator.filterNot(_.node.observed).map(DirectedEdge(_, EdgeDirection.F2N)).toList
      scheduleAcc(firstEdges, Set(), firstEdges)
    }
  }

}
