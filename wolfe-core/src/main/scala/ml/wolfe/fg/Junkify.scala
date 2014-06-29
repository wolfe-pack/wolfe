package ml.wolfe.fg

import ml.wolfe.FactorGraph
import ml.wolfe.FactorGraph.Node

import scala.annotation.tailrec
import scala.collection.SortedSet
import scala.collection.immutable.Queue

/**
 * Created by luke on 17/06/14.
 */
object Junkify {
  var debug:Boolean = _
  type Clique = SortedSet[Node]
  object Edge {
    def apply[T](_1: T, _2: T) = new Edge(_1, _2)
    def unapply[T](e: Edge[T]): Option[(T, T)] = Some(e._1, e._2)
  }
  class Edge[T](_1: T, _2: T) extends Tuple2[T, T](_1, _2) {
    override def equals(other: Any) = other match {
      case that: Edge[T] => super.equals(that) || super.equals(that.swap)
      case _ => false
    }
  }

  def apply(fg: FactorGraph, debug:Boolean = false) = {
    Junkify.debug = debug
    val nodes = fg.nodes.toSet

    def printGraph(title: String, neighbours: Map[Node, Set[Node]]) = {
      println(title)
      println(nodes.toSeq.sortBy(_.index).map(n =>
        neighbours(n).filter(_.index > n.index).toSeq.sortBy(_.index)
        .map(m => n + "-" + m).mkString("\t")
      ).mkString("\n"))
      println()
    }


    // MORALIZATION:
    val moralisedNeighbours: Map[Node, Set[Node]] = nodes.map(n =>
      n -> (
           for (
             e1 <- n.edges;
             e2 <- e1.f.edges;
             if e2 != e1
           ) yield e2.n
           ).toSet
    ).toMap
    if(debug) printGraph("MORALIZED", moralisedNeighbours)


    // TRIANGULATION:
    val triangulatedNeighbours = triangulate(nodes, moralisedNeighbours)
    if(debug) printGraph("TRIANGULATED", triangulatedNeighbours)


    // MAXIMAL CLIQUE GRAPH:   http://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
    val cliques = maximalCliques(nodes, triangulatedNeighbours)
    if(debug) {
      println("\nMaximal Cliques:")
      for (c <- cliques) println(c.mkString(", "))
      println()
    }


    // JUNCTION TREE:  Max spanning tree of clique graph, with weights given by intersection size
    val allCliqueEdges: Set[Edge[Clique]] = (
                                            for(x <- cliques.subsets(2) if (x.head & x.last).nonEmpty) yield new Edge(x.head, x.last)
                                            ).toSet
    val jtEdges = maxSpanningForest[Clique, Int](cliques, allCliqueEdges, e => productDimension(e._1 & e._2)) //is this what I want to be doing?)


    // CLIQUE POTENTIALS
    @tailrec
    val cliqueFactors = getCliqueFactors(cliques, fg.factors, cliques.map(_ -> Set[FactorGraph.Factor]()).toMap)
    if(debug) println("\nClique Factors:\n" + cliques.map(c => c + "\t\t has factors \t\t" + cliqueFactors(c)).mkString("\n"))


    // CONVERT TO A FACTOR GRAPH
    val jt = toFactorGraph(cliques, cliqueFactors, jtEdges)
    if(debug) println("\nJunkify Complete.")
    jt
  }




  def productDimension(nodes: Iterable[Node]) = nodes.foldLeft(1)(_ * _.variable.asDiscrete.dim)

  @tailrec
  def triangulate(remaining: Set[Node], neighboursAcc: Map[Node, Set[Node]]): Map[Node, Set[Node]] = {
    if (remaining.isEmpty) neighboursAcc
    else {

      def neighbourDimensions(n: Node): Int = productDimension(neighboursAcc(n))
      def isSimplical(n: Node): Boolean = {
        val r = neighboursAcc(n) & remaining

        r forall (x =>
          (r - x) forall (y =>
            neighboursAcc(x) contains y
          ))
      }
      def withNodeEliminated(n: Node): Map[Node, Set[Node]] = neighboursAcc ++
      neighboursAcc(n).map(m =>
        m -> (neighboursAcc(m) ++ neighboursAcc(n) - m)
      )

      val simplicalNodes = remaining filter isSimplical

      if (simplicalNodes.nonEmpty) {
        if(debug) println("Eliminating simplical nodes:\t" + simplicalNodes.mkString(", "))
        triangulate(remaining -- simplicalNodes, neighboursAcc)
      } else {
        val nodeToEliminate = remaining minBy neighbourDimensions
        if(debug) println("Eliminating " + nodeToEliminate + "\t(neighbour Dimension = " + neighbourDimensions(nodeToEliminate) + ")")
        triangulate(remaining - nodeToEliminate, withNodeEliminated(nodeToEliminate))
      }
    }
  }


  def maximalCliques(nodes: Set[Node], neighbours: Map[Node, Set[Node]]): Set[Clique] = {
    if(debug) println("Bron Kerbosch:")

    @tailrec
    def BronKerbosch2Acc(todo: Seq[(SortedSet[Node], Set[Node], Set[Node])], acc: Set[Clique]): Set[Clique] = todo match {
      case Nil => acc
      case (r, p, x) :: tail => (p ++ x).headOption match {
        case None => {
          if(debug) println(">> " + List(r, p, x).map(_.map(_.index).mkString(",")))
          BronKerbosch2Acc(tail, acc + r)
        }
        case Some(u) => {
          if(debug) println(List(r, p, x).map(_.map(_.index).mkString(",").padTo(nodes.map(_.index).mkString(",").length, ' ')).mkString(" | "))

          // State is a tuple: ((State for accumulator), Modified p and x for inner loop)
          def states = (p -- neighbours(u)).scanLeft ((r, p, x), p, x) {
            case ((_, p2, x2), v) => (
            (r + v, p2 & neighbours(v), x2 & neighbours(v)),  // State to remember for accumulator
            p2 - v, x2 + v                                    // Modified p and x for inner loop
            )
          }
          val next = states.map(_._1) - ((r, p, x))
          BronKerbosch2Acc(next ++: tail, acc)
        }
      }
    }

    BronKerbosch2Acc(Seq((SortedSet[Node]()(Ordering.by(_.index)), nodes, Set[Node]())), Set[Clique]())
  }




  def maxSpanningForest[T, S <% Ordered[S]](nodes:Set[T], edges: Set[Edge[T]], weight: Edge[T] => S): Set[Edge[T]] = {
    if(debug) println("Kruskal's:")
    @tailrec
    def kruskals(remainingEdges: Seq[Edge[T]], trees: Set[Set[T]], acc: Set[Edge[T]]): Set[Edge[T]] =
      remainingEdges.headOption match {
        case None => acc
        case Some(Edge(n1,n2)) => {
          def e = remainingEdges.head
          def tail = remainingEdges.tail
          val tree1 = trees.find(_ contains n1) match { case Some(x) => x; case None => sys.error("Something went wrong in Kruskal's!")}
          if(tree1 contains n2) kruskals(tail, trees, acc)
          else {
            if(debug) println("Adding Edge: " + n1.toString.padTo(40, ' ')  + " -- " + n2.toString.padTo(40, ' ') + " weight = " + weight(e))
            val tree2 = trees find(_ contains n2) match { case Some(x) => x; case None => sys.error("Something went wrong in Kruskal's!")}
            val newTrees = trees - tree1 - tree2 + (tree1 ++ tree2)
            kruskals(tail, newTrees, acc + e)
          }
        }
      }
    val edgesSorted = edges.toSeq.sortBy(weight).reverse
    kruskals(edgesSorted, nodes.map(Set(_)), Set())
  }


  def getCliqueFactors(cliques:Set[Clique], factors: Seq[FactorGraph.Factor], acc: Map[Clique, Set[FactorGraph.Factor]]): Map[Clique, Set[FactorGraph.Factor]] =
    factors.headOption match {
      case None => acc
      case Some(f) => {
        def f = factors.head
        val clique = cliques.find(c => f.edges.map(_.n).toSet.subsetOf(c))
        clique match {
          case Some(c) => getCliqueFactors(cliques, factors.tail, acc + (c -> (acc(c) + f)))
          case None => sys.error("Junction Tree creation error: a factor has no clique to contain its potential")
        }
      }
    }



  def toFactorGraph(cliques: Set[Clique], cliqueFactors: Map[Clique, Set[FactorGraph.Factor]], edges:Set[Edge[Clique]]) = {
    val jt = new FactorGraph
    val cliqueNodes = cliques.map(c => c -> jt.addTupleNode(c.toArray)).toMap

    for((clique, factors) <- cliqueFactors; f <- factors) {
      val wrapped = jt.addFactor()
      val edge = jt.addTupleEdge(wrapped, cliqueNodes(clique), f.edges.map(_.n.variable.asDiscrete))
      wrapped.potential = new WrappedPotential(f.potential, edge, f.edges.map(_.n.variable.asDiscrete))
    }

    for (e <- edges) {
      val f = jt.addFactor()

      val intersectionVariables = (e._1 & e._2).toArray.map(_.variable.asDiscrete)

      val edge1 = jt.addTupleEdge(f, cliqueNodes(e._1), intersectionVariables)
      val edge2 = jt.addTupleEdge(f, cliqueNodes(e._2), intersectionVariables)

      f.potential = new TupleConsistencyPotential(edge1, edge2)
    }
    jt.build()

    jt
  }
}