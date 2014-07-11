package ml.wolfe.fg

import ml.wolfe.FactorGraph
import ml.wolfe.FactorGraph.Node

import scala.annotation.tailrec
import scala.collection.SortedSet
import scala.collection.immutable.Queue

/**
 * @author luke
 */
object Junkify {
  /**
   * An undirected Edge between two objects of type T
   */
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
  type Clique = SortedSet[Node]

  /**
   * Given a factor graph, return a corresponding junction tree
   * @param fg the original factor graph
   * @return the junction tree (as a factor graph)
   */
  def apply(fg: FactorGraph) = {
    val nodes = fg.nodes.toSet

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

    // TRIANGULATION:
    val triangulatedNeighbours = triangulate(nodes, moralisedNeighbours)

    // MAXIMAL CLIQUE GRAPH:
    val cliques = maximalCliques(nodes, triangulatedNeighbours)

    // JUNCTION TREE:  Max spanning tree of clique graph, with weights given by intersection size
    val allCliqueEdges: Set[Edge[Clique]] = (
                                            for(x <- cliques.subsets(2) if (x.head & x.last).nonEmpty) yield new Edge(x.head, x.last)
                                            ).toSet
    val jtEdges = maxSpanningForest[Clique, Int](cliques, allCliqueEdges, e => productDimension(e._1 & e._2)) //is this what I want to be doing?)


    // CLIQUE POTENTIALS
    val cliqueFactors = getCliqueFactors(cliques, fg.factors)

    // CONVERT TO A FACTOR GRAPH
    val jt = toFactorGraph(cliques, cliqueFactors, jtEdges)

    jt
  }




  def productDimension(nodes: Iterable[Node]) = nodes.foldLeft(1)(_ * _.variable.asDiscrete.dim)

  /**
   * Given an undirected graph of nodes, triangulate it
   * @param nodes the nodes on the graph
   * @param neighbours A map from each node to its neighbours in the original graph
   * @return the new map from each node to its (triangulated) neighbours
   */
  @tailrec
  def triangulate(nodes: Set[Node], neighbours: Map[Node, Set[Node]]): Map[Node, Set[Node]] = {
    if (nodes.isEmpty) neighbours
    else {

      def neighbourDimensions(n: Node): Int = productDimension(neighbours(n))
      def isSimplical(n: Node): Boolean = {
        val r = neighbours(n) & nodes

        r forall (x =>
          (r - x) forall (y =>
            neighbours(x) contains y
          ))
      }
      def withNodeEliminated(n: Node): Map[Node, Set[Node]] = neighbours ++
      neighbours(n).map(m =>
        m -> (neighbours(m) ++ neighbours(n) - m)
      )

      val simplicalNodes = nodes filter isSimplical

      if (simplicalNodes.nonEmpty) {
        triangulate(nodes -- simplicalNodes, neighbours)
      } else {
        val nodeToEliminate = nodes minBy neighbourDimensions
        triangulate(nodes - nodeToEliminate, withNodeEliminated(nodeToEliminate))
      }
    }
  }

  /**
   * Given an undirected graph of nodes, find the maximal cliques
   * @see [[http://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm Bron Kerbosch algorithm]]
   * @param nodes the nodes of the graph
   * @param neighbours A map from each node to its neighbours in the original graph
   * @return A set of cliques (sets of nodes) in the graph
   */
  def maximalCliques(nodes: Set[Node], neighbours: Map[Node, Set[Node]]): Set[Clique] = {
    @tailrec
    def BronKerbosch2Acc(todo: Seq[(SortedSet[Node], Set[Node], Set[Node])], acc: Set[Clique]): Set[Clique] = todo match {
      case Nil => acc
      case (r, p, x) :: tail => (p ++ x).headOption match {
        case None => BronKerbosch2Acc(tail, acc + r)
        case Some(u) =>
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

    BronKerbosch2Acc(Seq((SortedSet[Node]()(Ordering.by(_.index)), nodes, Set[Node]())), Set[Clique]())
  }

  /**
   * Given an undirected (not necessarily connected) graph, find the maximal spanning forest
   * @param nodes the nodes of the graph
   * @param edges the edges of the graph
   * @param weight a map from edges to their weights
   * @tparam T the type of the vertices in the graph
   * @tparam S the type of the weights
   * @return
   */
  def maxSpanningForest[T, S <% Ordered[S]](nodes:Set[T], edges: Set[Edge[T]], weight: Edge[T] => S): Set[Edge[T]] = {
    @tailrec
    def kruskals(remainingEdges: Seq[Edge[T]], trees: Set[Set[T]], acc: Set[Edge[T]]): Set[Edge[T]] =
      remainingEdges.headOption match {
        case None => acc
        case Some(Edge(n1,n2)) =>
          def e = remainingEdges.head
          def tail = remainingEdges.tail
          val tree1 = trees.find(_ contains n1) match { case Some(x) => x; case None => sys.error("Something went wrong in Kruskal's!")}
          if(tree1 contains n2) kruskals(tail, trees, acc)
          else {
            val tree2 = trees find(_ contains n2) match { case Some(x) => x; case None => sys.error("Something went wrong in Kruskal's!")}
            val newTrees = trees - tree1 - tree2 + (tree1 ++ tree2)
            kruskals(tail, newTrees, acc + e)
        }
      }
    val edgesSorted = edges.toSeq.sortBy(weight).reverse
    kruskals(edgesSorted, nodes.map(Set(_)), Set())
  }

  /**
   * Given a set of cliques (sets of nodes) and factors, assign each factor to a
   * clique that contains all of the relevant nodes
   * @param cliques the cliques
   * @param factors the factors
   * @return a map from cliques to the factors that they are responsible for
   */
  def getCliqueFactors(cliques:Set[Clique], factors: Seq[FactorGraph.Factor]): Map[Clique, Set[FactorGraph.Factor]] = {
    @tailrec
    def getCliqueFactorsAcc(cliques: Set[Clique], factors: Seq[FactorGraph.Factor], acc: Map[Clique, Set[FactorGraph.Factor]])
      : Map[Clique, Set[FactorGraph.Factor]] = factors.headOption match {
        case None => acc
        case Some(f) =>
          def f = factors.head
          val clique = cliques.find(c => f.edges.map(_.n).toSet.subsetOf(c))
          clique match {
            case Some(c) => getCliqueFactorsAcc(cliques, factors.tail, acc + (c -> (acc(c) + f)))
            case None => sys.error("Junction Tree creation error: a factor has no clique to contain its potential")
          }
      }
    getCliqueFactorsAcc(cliques, factors, cliques.map(_ -> Set[FactorGraph.Factor]()).toMap)
  }

  /**
   * Create a factor graph to represent the junction tree
   * @param cliques the cliques of the original graph (nodes of the junction tree)
   * @param cliqueFactors a map from cliques to the factors they should hold
   * @param edges the edges of the original graph
   * @return the junction tree, as a factor graph
   */
  def toFactorGraph(cliques: Set[Clique], cliqueFactors: Map[Clique, Set[FactorGraph.Factor]], edges:Set[Edge[Clique]]) = {
    val jt = new FactorGraph
    // a map from cliques in the fg to nodes in the jt
    val jtNodes = cliques.map(c => c -> jt.addTupleNode(c.toArray)).toMap

    for((clique, factors) <- cliqueFactors if factors.nonEmpty) {
      val components = factors.toArray
      val groupFactor = jt.addFactor()
      val baseNodes = components.flatMap(_.edges.map(_.n)).distinct.sortBy(_.index)
      val edge = jt.addTupleEdge(groupFactor, jtNodes(clique), baseNodes)
      groupFactor.potential = new GroupPotential(components, edge, baseNodes)
    }

    for (e <- edges) e match {
      case Edge(clique1, clique2) =>
        val f = jt.addFactor()
        val intersectionNodes = (clique1 & clique2).toArray
        val edge1 = jt.addTupleEdge(f, jtNodes(clique1), intersectionNodes)
        val edge2 = jt.addTupleEdge(f, jtNodes(clique2), intersectionNodes)
        f.potential = new TupleConsistencyPotential(edge1, edge2)
    }

    jt.build()
    jt

  }
}