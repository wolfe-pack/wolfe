package ml.wolfe

import java.io.PrintWriter

import cc.factorie.la.SingletonTensor
import ml.wolfe.util.Multidimensional._

import scala.collection.mutable.ArrayBuffer
import scalaxy.loops._
import scala.annotation.tailrec
import scala.collection.mutable
import ml.wolfe.fg._


/**
 * A simple Factor Graph with infrastructure for message passing.
 */
final class FactorGraph {

  import FactorGraph._

  /**
   * Edges from factors to nodes.
   */
  val edges = new ArrayBuffer[Edge]

  /**
   * Nodes that represent variables.
   */
  val nodes = new ArrayBuffer[Node]

  /**
   * Factors that capture model potentials.
   */
  val factors = new ArrayBuffer[Factor]

  /**
   * factors used only to calculate expectations.
   */
  val expectationFactors = new ArrayBuffer[Factor]

  /**
   * Edges between expectation factors and nodes.
   */
  val expectationEdges = new ArrayBuffer[Edge]

  /**
   * Stochastic factors can sample their neighbors.
   */
  val stochasticFactors = new ArrayBuffer[(Factor, () => Seq[Node])]()

  /**
   * A flag that indicates whether the algorithm that uses this graph to find a solution has converged
   */
  var converged: Boolean = false

  /**
   * Linear factors can depend on a weight vector to calculate their scores.
   */
  var weights: FactorieVector = null

  /**
   * Algorithms that use message passing to calculate a value (maximum, log partition function ...) can store the
   * result here.
   */
  var value = 0.0

  /**
   * Algorithms that calculate gradients (or sub-gradients) can store their results here.
   */
  var gradient: FactorieVector = null

  /**
   * Algorithms can use this variable to store expectations.
   */
  var expectations: FactorieVector = null

  /**
   * Adds a node for a variable of domain size `dim`
   * @param dim size of domain of corresponding variable
   * @param label description of what the variable represents
   * @param domainLabels description of each element in the domain
   * @return the added node.
   */
  def addNode(dim: Int, label:String = "", domainLabels:Seq[String]=Seq()) = {
    val n = new Node(nodes.size, new DiscreteVar(dim, label, domainLabels))
    nodes += n
    n
  }

  /**
   * Adds a tuple node (with components probably from another factor graph)
   * @param componentNodes the components of the tuple
   * @return the added tuple node.
   */
  def addTupleNode(componentNodes: Array[Node]) = {
    val variable = new TupleVar(componentNodes)
    val n = new Node(nodes.size, variable)
    nodes += n
    n
  }

  /**
   * Adds an edge with DiscreteMsgs between node and factor
   * @param f factor to connect.
   * @param n node to connect.
   * @param indexInFactor the index the edge has in the factor.
   * @return the added edge.
   */
  def addEdge(f: Factor, n: Node, indexInFactor: Int): Edge = {
    val e = new Edge(n, f, new DiscreteMsgs(n.variable.asDiscrete.dim))
    e.indexInFactor = indexInFactor
    n.edgeCount += 1
    f.edgeCount += 1
    edges += e
    e
  }

  /**
   * Adds an edge between an expectation factor and a node. This does not register
   * the factor edge in the node!
   * @param f factor to connect.
   * @param n node to connect.
   * @return the added edge.
   */
  def addExpectationEdge(f: Factor, n: Node, indexInFactor: Int): Edge = {
    val e = new Edge(n, f, new DiscreteMsgs(n.variable.asDiscrete.dim))
    e.indexInFactor = f.edgeCount
    f.edgeCount += 1
    expectationEdges += e
    e
  }

  /**
   * Adds an edge between node and factor
   * @param f factor to connect.
   * @param n node to connect.
   * @return the added edge.
   */
  def addEdge(f: Factor, n: Node): Edge = addEdge(f, n, f.edgeCount)

  /**
   * Adds an edge with TupleMsgs between tuple node and factor
   * @param f factor to connect.
   * @param n node to connect.
   * @param msgNodes the nodes relevant for f2n messages
   * @return the added edge.
   */
  def addTupleEdge(f: Factor, n: Node, msgNodes: Array[Node]): Edge = {
    val e = new Edge(n, f, new TupleMsgs(n.variable.asTuple.components, msgNodes.map(_.variable.asDiscrete)))
    e.indexInFactor = f.edgeCount
    n.edgeCount += 1
    f.edgeCount += 1
    edges += e
    e
  }

  /**
   * Creates a new factor, no potential assigned.
   * @return the created factor.
   */
  def addFactor(label:String = "") = {
    val f = new Factor(this, factors.size, label)
    factors += f
    f
  }

  /**
   * Creates and adds a factor to be used for calculating expectations.
   * @return the created factor.
   */
  def addExpectationFactor(label:String = "") = {
    val f = new Factor(this, expectationFactors.size, label)
    expectationFactors += f
    f
  }

  /**
   * Adds a factor whose nodes will be resampled.
   * @param sampleNodes a function that samples neighbors of the factor
   */
  def addStochasticFactor(sampleNodes: => Seq[Node]) {
    val f = addFactor()
    stochasticFactors += f -> (() => sampleNodes)
  }

  /**
   * Change the arguments of each stochastic factor.
   */
  def sampleFactors() {
    for ((f, s) <- stochasticFactors) {
      moveFactor(f, s())
    }
  }

  /**
   * Removes the factor from the graph. Very expensive.
   * @param factor the factor to remove.
   */
  def removeFactor(factor: Factor) {
    for (e <- factor.edges) {
      e.n.edges = e.n.edges.filter(_ != e)
      e.n.edgeCount -= 1
      edges.remove(edges.indexOf(e))
    }
    factors.remove(factors.indexOf(factor))
  }

  /**
   * Method to efficiently "move" a factor from one set of nodes
   * to another. Factor will retain its potential and edges, only
   * the edge nodes are changed.
   * @param factor factor to move.
   * @param newNodes the new nodes the factor should point to. It should be the same number and types of
   *                 variables as the factor connected before.
   */
  def moveFactor(factor: Factor, newNodes: Seq[Node]) {
    require(newNodes.size == factor.edges.size)
    for ((e, n) <- factor.edges zip newNodes) {
      //todo: some of the array recreation code may be a bottleneck.
      e.n.edges = e.n.edges.filter(_ != e)
      e.n.edgeCount -= 1
      e.n = n
      e.n.edgeCount += 1
      e.n.edges = e.n.edges :+ e
    }
  }

  /**
   * creates node message buffers and domains if necessary.
   */
  def setupNodes() {
    for (node <- nodes) node.variable.setup()
  }

  /**
   * Connecting nodes and factors to the edges between them.
   */
  def build() {
    setupNodes()
    for (edge <- edges) {
      if (edge.f.edges.length != edge.f.edgeCount) edge.f.edges = Array.ofDim[Edge](edge.f.edgeCount)
      if (edge.n.edges.length != edge.n.edgeCount) edge.n.edges = Array.ofDim[Edge](edge.n.edgeCount)
      edge.indexInNode = edge.n.edgeFilled
      edge.f.edges(edge.indexInFactor) = edge
      edge.n.edges(edge.indexInNode) = edge
      edge.f.edgeFilled += 1
      edge.n.edgeFilled += 1
    }
    //expectation edges are not registered in nodes.
    for (edge <- expectationEdges) {
      if (edge.f.edges.length != edge.f.edgeCount) edge.f.edges = Array.ofDim[Edge](edge.f.edgeCount)
      edge.f.edges(edge.indexInFactor) = edge
      edge.f.edgeFilled += 1
    }
  }


  /**
   * Verbose string representation of the graph.
   * @param fgPrinter a printer for nodes and factors.
   * @return string representation of graph.
   */
  def toVerboseString(fgPrinter: FGPrinter = DefaultPrinter) = {
    f"""
      |Nodes:
      |${ nodes.map(_.toVerboseString(fgPrinter.node2String)).mkString("\n") }
      |
      |Factors:
      |${ factors.map(_.toVerboseString(fgPrinter)).mkString("\n") }
      |
      |Edges:
      |${ edges.map(_.toVerboseString(fgPrinter)).mkString("\n") }
    """.stripMargin
  }


  def getNode(index: Int) = nodes(index)

  def getFactor(index: Int) = factors(index)

  /**
   * @return Is this factor graph loopy?
   */
  def isLoopy: Boolean = {
    @tailrec
    def loopyAcc(remainingFactors: List[Factor], trees: Set[Set[Node]]): Boolean =
      remainingFactors match {
        case Nil => false
        case f :: tail => {
          val neighbourTrees = f.edges.map { e => trees.find(_ contains e.n) match { case Some(x) => x; case None => sys.error("Something went wrong in isLoopy!") } }.toSet
          if (neighbourTrees.size != f.edges.length) true
          else {
            val newTrees = trees -- neighbourTrees + neighbourTrees.reduce(_ ++ _)
            loopyAcc(tail, newTrees)
          }
        }
      }
    loopyAcc(factors.toList, nodes.map(Set(_)).toSet)
  }


  def d3Code : String = {
    def escape(s:String) =
      s.replace("\n","\\n").replace("\'", "\\\'")

    val fgid = this.hashCode().toString

    val genCode =  s"""
        |var FG$fgid = {graph:{
        |  "nodes": [${(
              nodes.map(n =>
                     "{text:'" + escape(n.variable.label) + "'" +
              ", hoverhtml:'Domain: {" + n.variable.asDiscrete.domainLabels.mkString(", ") + "}'" +
              "}") ++
              factors.map(f =>
                "{shape: 'square'" +
                ", hoverhtml:'" + "<div class=\"tooltipheader\">" + escape(f.label) + "</div>" +
                  escape(f.potential.toVerboseString(DefaultPrinter)) + "'" +
              "}")
          ).mkString(", ")}
        |  ],
        |  "links": [
        |    ${edges.map(e =>
                "{'source': " + e.n.index + ", 'target': " + (e.f.index + nodes.length) + "}"
              ) mkString ", "}
        |  ]
        |}}
      """.stripMargin

    val code =
      s"""
        |<div id="FG$fgid">
        |<script src="http://d3js.org/d3.v3.min.js"></script>
        |<style type="text/css">
        |
        |.link {
        |	stroke: #000;
        |	stroke-width: 1.5px;
        |}
        |
        |.node {
        |	cursor: move;
        |	fill: #ccc;
        |	stroke: #000;
        |	stroke-width: 1.5px;
        |}
        |
        |.label {
        |	cursor: move;
        |	font-size: 15px;
        |	font-family: 'sans-serif';
        |	pointer-events:none;
        |	-moz-user-select: -moz-none;
        |	-khtml-user-select: none;
        |	-webkit-user-select: none;
        |	-o-user-select: none;
        |	user-select: none;
        |}
        |
        |.tooltip {
        |	padding:0px;
        |	font-family: 'sans-serif';
        |	pointer-events:none;
        |	-moz-user-select: -moz-none;
        |	-khtml-user-select: none;
        |	-webkit-user-select: none;
        |	-o-user-select: none;
        |	user-select: none;
        |}
        |
        |/*
        |.tooltipbox {
        |	pointer-events:none;
        |	fill:#eee;
        |	fill-opacity:.9;
        |	stroke:#666;
        |	stroke-width:1.5px;
        |}*/
        |
        |.tooltipinner {
        |	pointer-events:none;
        |	background:#eee;
        |	border:1.5px solid;
        |	border-radius:10px;
        |	overflow:hidden;
        |	padding:5px;
        |	max-height:400px;
        |}
        |
        |.tooltipheader {
        |	background-color:white;
        |	border-bottom:gray 1.5px solid;
        |	padding:5px;
        |	margin:-5px -5px 0 -5px;
        |	font-style:italic;
        |}
        |
        |.potentialtable {
        |	border-spacing: 0px;
        |	text-align: center;
        |	margin-left: auto;
        |	margin-right: auto;
        |}
        |
        |.potentialtable td {
        |	padding-top:3px;
        |	padding-left:15px;
        |}
        |
        |</style>
        |
        |
        |
        |<script>
        |$genCode
        |</script>
        |
        |<script type="text/javascript">
        |FG$fgid.width = 500,
        |FG$fgid.height = 400;
        |
        |FG$fgid.force = d3.layout.force()
        |.size([FG$fgid.width, FG$fgid.height])
        |.charge(-3000)
        |.gravity(0.5)
        |    //.linkDistance(150)
        |    //.on("tick", tick);
        |
        |    FG$fgid.drag = FG$fgid.force.drag()
        |
        |    FG$fgid.svg = d3.select("#FG$fgid").append("svg")
        |    .attr("width", FG$fgid.width)
        |    .attr("height", FG$fgid.height);
        |
        |
        |    FG$fgid.link = FG$fgid.svg.selectAll(".link")
        |    FG$fgid.node = FG$fgid.svg.selectAll(".node")
        |    FG$fgid.label = FG$fgid.svg.selectAll(".label");
        |
        |    FG$fgid.force
        |    .nodes(FG$fgid.graph.nodes)
        |    .links(FG$fgid.graph.links)
        |    .start();
        |
        |    FG$fgid.link = FG$fgid.link.data(FG$fgid.graph.links)
        |	    .enter().append("line")
        |	    .attr("class", "link");
        |
        |	FG$fgid.node = FG$fgid.node.data(FG$fgid.graph.nodes)
        |	    .enter().append("path")
        |	    .attr("class", "node")
        |	    .attr("d", d3.svg.symbol()
        |	    	.type(function(d) { return d.shape == undefined ? "circle" : d.shape })
        |	    	.size(2000))
        |	    .on("mouseover", function(d){
        |	    	if(d.hoverhtml != undefined) {
        |	    		FG$fgid.setTooltip(d.hoverhtml);
        |	    		FG$fgid.tooltip.transition()
        |	    			.duration(300)
        |	    			.style("opacity", .9);
        |	    		FG$fgid.tooltipNode = d;
        |	    		FG$fgid.moveTooltip();
        |	    	}
        |	    })
        |	    .on("mouseout", function(d){
        |			FG$fgid.tooltip.transition()
        |	                .duration(300)
        |	                .style("opacity", 0)
        |	    })
        |		.call(FG$fgid.drag);
        |
        |FG$fgid.label = FG$fgid.label.data(FG$fgid.graph.nodes)
        |	.enter().append("text")
        |	.attr("class", "label")
        |	.attr("dy", "5")
        |	.attr("text-anchor", "middle")
        |	.text(function(d) { return d.text == undefined ? "" : d.text })
        |	.call(FG$fgid.drag);
        |
        |FG$fgid.tooltipNode = null
        |FG$fgid.tooltip = null
        |
        |while(FG$fgid.force.alpha() != 0) {
        |	FG$fgid.force.tick();
        |}
        |
        |FG$fgid.tick = function() {
        |	FG$fgid.link.attr("x1", function(d) { return d.source.x; })
        |		.attr("y1", function(d) { return d.source.y; })
        |		.attr("x2", function(d) { return d.target.x; })
        |		.attr("y2", function(d) { return d.target.y; });
        |
        |	FG$fgid.node.attr("transform", function(d) {return "translate(" + d.x + "," + d.y + ")"});
        |	FG$fgid.label.attr("transform", function(d) {return "translate(" + d.x + "," + d.y + ")"});
        |	FG$fgid.moveTooltip();
        |
        |}
        |
        |FG$fgid.setTooltip = function(html) {
        |	if(FG$fgid.tooltip != null) {
        |		FG$fgid.tooltip.remove()
        |	}
        |	FG$fgid.tooltip = FG$fgid.svg.insert("foreignObject")
        |		.attr("class", "tooltip")
        |		.attr("width", "300")
        |		.attr("height", "100%")
        |		.style("opacity", 0)
        |		.html("<div class='tooltipinner'>" + html + "</div>")
        |}
        |
        |
        |FG$fgid.moveTooltip = function() {
        |	if(FG$fgid.tooltipNode != null) {
        |		FG$fgid.tooltip.attr("transform", "translate(" + (FG$fgid.tooltipNode.x-150) + "," + (FG$fgid.tooltipNode.y+15) + ")" );
        |	}
        |}
        |
        |FG$fgid.force.on("tick", FG$fgid.tick);
        |FG$fgid.tick();
        |</script>
        |</div>
      """.stripMargin

    code
  }

}

object FactorGraph {


  /**
   * A node representing a variable.
   * @param index the index of the node.
   * @param variable the variable the node is representing.
   */
  final class Node(val index: Int, var variable: Var) {
    /* all edges to factors that this node is connected to */
    var edges: Array[Edge] = Array.ofDim(0)

    def toVerboseString(nodePrinter: Node => String = n => "") = {
      f"""-----------------
        |Node:   $index%3d ${ nodePrinter(this) }
        |Var:    $variable
      """.stripMargin
    }

    override def toString = "N" + index.toString

    private[FactorGraph] var edgeCount : Int = 0
    private[FactorGraph] var edgeFilled: Int = 0

    override def equals(other: Any): Boolean = other match {
      case that: Node =>
        index == that.index
      case _ => false
    }
    override def hashCode(): Int = {
      val state = Seq(index)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  /**
   * An edge between node and factor
   * @param n the node.
   * @param f the factor.
   * @param msgs the Msgs used for message passing
   */
  final class Edge(var n: Node, val f: Factor, var msgs: Msgs) {
    var indexInFactor = -1
    var indexInNode   = -1

    def toVerboseString(fgPrinter: FGPrinter) =
      f"""----------
        |Edge
        |Node:    ${ n.index } ${ fgPrinter.node2String(n) }
        |Factor:  ${ f.index } ${ fgPrinter.factor2String(f) }
      """.stripMargin
    override def toString = s"${ f.index } -> ${ n.index }"
  }

  /**
   * A factor in a message passing factor graph.
   *
   * Some hints for the usage of Factors of FactorGraph
   * f.potential.table.settings    | f.potential.table.scores
   * 0 0 0         | 0.5
   * 0 0 1         | 0.8
   * 0 0 2         | 0.9
   * 0 1 0         | 1.0
   * 0 1 1         | 0.1
   * 0 1 2         | 0.2
   * print(index) = 3
   * print(dims)  = 1,2,3
   * Node 0:  factor.edges(0).n
   * Node 1:  factor.edges(1).n
   * Node 2:  factor.edges(2).n
   *
   * @param fg the factor graph.
   * @param index the index/id of the factor
   */
  final class Factor(val fg: FactorGraph, val index: Int, val label:String = "") {
    var edges: Array[Edge] = Array.ofDim(0)

    def numNeighbors = edges.size

    /**
     * The potential for this factor. Usually created after edges to factor have been created as
     * the potential directly works with the edges.
     */
    var potential: Potential = null

    /**
     * More verbose string representation that shows that potential table depending on factor type.
     * @param fgPrinter a printer that can print nodes and factors.
     * @return A verbose string representation of this factor.
     */
    def toVerboseString(implicit fgPrinter: FGPrinter) = {
      val tableString = potential.toVerboseString
      f"""-----------------
        |Factor:  $index ${ fgPrinter.factor2String(this) }
        |Nodes:   ${ edges.map(_.n.index).mkString(" ") } ${ edges.map(e => fgPrinter.node2String(e.n)).mkString(" ") }
        |Table:
        |${ tableString }
      """.stripMargin
    }

    override def toString = index.toString

    private[FactorGraph] var edgeCount : Int = 0
    private[FactorGraph] var edgeFilled: Int = 0

    override def equals(other: Any): Boolean = other match {
      case that: Factor =>
        index == that.index
      case _ => false
    }
    override def hashCode(): Int = {
      val state = Seq(index)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }


  object EdgeDirection extends Enumeration {
    type EdgeDirection = Value
    val N2F, F2N = Value
  }
  import EdgeDirection._
  case class DirectedEdge(edge:Edge, direction:EdgeDirection) {
    def f = edge.f
    def n = edge.n
    def swap = DirectedEdge(edge, if(direction == N2F) F2N else N2F)
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

    def schedule(node:Node):Seq[DirectedEdge] = {
      val forward = scheduleForward(node)
      val backward = forward.reverse.map(_.swap)
      forward ++ backward
    }

    /**
     * Runs scheduler on all disconnected components of the graph
     * @param graph factor graph with (possibly) disconnected components
     * @return schedule for forward-backward over all disconnected components of the graph
     */
    def schedule(graph: FactorGraph):Seq[DirectedEdge] = {
      val forward = scheduleForward(graph)
      val backward = forward.reverse.map(_.swap)
      forward ++ backward
    }

    def scheduleForward(graph: FactorGraph): Seq[DirectedEdge] = {
      @tailrec
      def scheduleAcc(nodes: Seq[Node], done: Set[Node], acc: Seq[DirectedEdge]): Seq[DirectedEdge] = nodes match {
        case Nil => acc
        case head :: tail =>
          if (done.contains(head)) scheduleAcc(tail, done, acc)
          else {
            val edges = scheduleForward(head)
            scheduleAcc(tail, done ++ edges.map(_.n), acc ++ edges)
          }
      }

      scheduleAcc(graph.nodes.toList, Set(), Seq())
    }

    def schedule(factor: Factor): Seq[DirectedEdge] = schedule(factor.edges.head)

    def schedule(edge: Edge): Seq[DirectedEdge] = schedule(edge.n)
  }

  object MPSchedulerImpl extends MPScheduler {
    /**
     * @param node The node which will become the root node
     * @return correct ordering for messaging pass for given direction (excluding the staring edge)
     */
    def scheduleForward(node: Node) : Seq[DirectedEdge] = {
      @tailrec
      def scheduleAcc(todo: List[DirectedEdge], done: Set[Edge], acc: Seq[DirectedEdge]): Seq[DirectedEdge] =
        todo match {
          case Nil => acc
          case head :: tail =>
            if (done.contains(head.edge)) scheduleAcc(tail, done, acc)
            else {
              val parents = head.direction match {
                case EdgeDirection.N2F => head.n.edges.
                                          filterNot(e => e == head.edge || todo.view.map(_.edge).contains(e)).
                                          map(DirectedEdge(_, EdgeDirection.F2N))
                case EdgeDirection.F2N => head.f.edges.
                                          filterNot(e => e == head.edge || todo.view.map(_.edge).contains(e)).
                                          map(DirectedEdge(_, EdgeDirection.N2F))
              }

              scheduleAcc(tail ++ parents, done + head.edge, parents ++ acc)
            }
        }
      val firstEdges = node.edges.map(DirectedEdge(_, EdgeDirection.F2N)).toList
      scheduleAcc(firstEdges, Set(), firstEdges)
    }


  }

  /**
   * Printer of nodes, factors and edges. Helps debugging (as the graph itself is
   * independent of the original model it comes from.
   */
  trait FGPrinter {
    def node2String(node: Node): String
    def factor2String(factor: Factor): String
    def vector2String(vector: FactorieVector): String
  }


  object DefaultPrinter extends FGPrinter {
    def node2String(node: Node) = node.variable match {
      case v: TupleVar => v.componentNodes.map(_.index.toString).mkString("(", ",", ")")
      case _ => ""
    }
    def factor2String(factor: Factor) = factor.potential match {
      case p: TuplePotential => p.baseNodes.map(_.index.toString).mkString("(", ",", ")")
      case _ => ""
    }
    def vector2String(vector: ml.wolfe.FactorieVector) = vector match {
      case v: SingletonVector => v.singleIndex + " -> " + v.singleValue
      case _ => vector.toString()
    }
  }


  /**
   * A mutable store for factor graphs. Can be combined with an inference method when
   * we want to expect the factor graph that the inference method used.
   */
  trait Store extends (FactorGraph => FactorGraph) {
    var factorGraph:FactorGraph = _
    def apply(v1: FactorGraph) = {
      factorGraph = v1
      factorGraph
    }
  }

  object Store extends Store

}

