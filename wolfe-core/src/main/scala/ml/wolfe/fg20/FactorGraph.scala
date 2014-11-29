package ml.wolfe.fg20

import ml.wolfe.FactorieVector

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag



/**
 * A factor graph is a bipartite graph with two types of vertices: nodes and factors.
 * This implementation is tailored to represent sums of potentials over a set of variables. Here each node represents one
 * variable, and each factor represents a potential.
 *
 * The FG class further supports different types of nodes: discrete, continuous, vector-based etc.
 *
 * FGs can be further customized to have specialized implementations of nodes, edges and factors. For example,
 * a FG subclass can use edges to store messages from nodes to factor.
 */
trait FactorGraph extends ProblemListener {

  fg =>

  /**
   * The type of potentials on the factors of this factor graph.
   */
  type Pot <: Potential

  /**
   * The type of processor used on factors
   */
  type Processor

  /**
   * The factor type specifies the type of factors used in the FG. Subclasses can use this to store
   * additional information in factors, such as statistics gathered in sampling, or a buffer that can be used
   * for efficient calculation of outgoing messages.
   */
  type FactorType <: Factor

  /**
   * Of what type are the edges between nodes and factors. Edges can contain messages, current gradients of variables
   * etc.
   */
  type EdgeType <: Edge

  /**
   * Of what type are the nodes in the factor graph. Subclasses can store additional information, such as inference
   * results or counts etc.
   */
  type NodeType <: Node

  /**
   * Edges between discrete nodes and factors.
   */
  type DiscEdge <: TypedEdge[DiscNode] with EdgeType

  /**
   * Edges between continuous nodes and factors.
   */
  type ContEdge <: TypedEdge[ContNode] with EdgeType

  /**
   * Edges between vector nodes and factors.
   */
  type VectEdge <: TypedEdge[VectNode] with EdgeType

  /**
   * A Node representing a discrete variable.
   */
  type DiscNode <: BasicDiscNode with NodeType

  /**
   * A Node representing a continuous variable.
   */
  type ContNode <: BasicContNode with NodeType

  /**
   * A Node representing a vector variable.
   */
  type VectNode <: BasicVectNode with NodeType

  trait BasicDiscNode extends TypedNode[DiscVar[Any], DiscEdge] {
    var setting = 0

    def observe(setting: Int, observed: Boolean = true): Unit = {
      this.setting = setting
      this.observed = observed
    }
  }

  trait BasicContNode extends TypedNode[ContVar, ContEdge] {
    var setting = 0.0
    def observe(setting: Double, observed: Boolean = true): Unit = {
      this.setting = setting
      this.observed = observed
    }

  }

  trait BasicVectNode extends TypedNode[VectVar, VectEdge] {
    var setting: FactorieVector = null
    def observe(setting: FactorieVector, observed: Boolean = true): Unit = {
      this.setting = setting
      this.observed = observed
    }

  }

  trait Node {
    def variable: Var[Any]
    def edgeList: List[EdgeType]
    def observed:Boolean
    override def toString = variable.name
  }

  abstract class TypedNode[V <: Var[Any], E <: EdgeType : ClassTag] extends Node {
    def variable: V
    var observed = false
    def edgeList = edges.toList
    var edges     : Array[E] = null
    var statsEdges: Array[E] = null

    private[FactorGraph] def build() {
      edges = buffer.toArray
      statsEdges = statsEdgeBuffer.toArray
    }
    private[FactorGraph] var buffer         : List[E] = Nil
    private[FactorGraph] var statsEdgeBuffer: List[E] = Nil

  }

  trait Edge {
    def node: NodeType
    def factor: FactorType
    def index: Int
  }

  trait TypedEdge[N <: NodeType] extends Edge {
    var node: N
  }

  def processor(pot:Pot):Processor

  abstract class Factor {
    val pot: Pot
    val processor: Processor       = fg.processor(pot)
    var discEdges: Array[DiscEdge] = null
    var contEdges: Array[ContEdge] = null
    var vectEdges: Array[VectEdge] = null
    def edges:Iterator[EdgeType] = discEdges.iterator ++ contEdges.iterator ++ vectEdges.iterator
    override def toString = edges.map(_.node).mkString("(",",",")")
  }

  def problem: Problem[Pot]

  def createDiscNode(v: DiscVar[Any]): DiscNode
  def createContNode(v: ContVar): ContNode
  def createVectNode(v: VectVar): VectNode
  def createDiscEdge(n: DiscNode, f: FactorType, index: Int): DiscEdge
  def createContEdge(n: ContNode, f: FactorType, index: Int): ContEdge
  def createVectEdge(n: VectNode, f: FactorType, index: Int): VectEdge
  def createFactor(pot: Pot): FactorType

  def createAndLinkFactor(p: Pot, linkEdges: Boolean = true) = {
    val factor = createFactor(p)
    createDiscEdges(factor, linkEdges)
    createContEdges(factor, linkEdges)
    createVectEdges(factor, linkEdges)
    factor
  }

  def nodes:Iterator[NodeType] = var2DiscNode.values.iterator ++ var2ContNode.values.iterator ++ var2VectNode.values.iterator
  def edges = factors.iterator.flatMap(_.edges)

  def discNodes = var2DiscNode.valuesIterator
  def contNodes = var2ContNode.valuesIterator
  def vectNodes = var2VectNode.valuesIterator

  def setObservations(state: State = problem.observation) {
    for (v <- state.domain) {
      v match {
        case d: DiscVar[_] => var2DiscNode(d).observe(d.dom.indexOf(problem.observation(d)))
        case c: ContVar => var2ContNode(c).observe(state(c))
        case v: VectVar => var2VectNode(v).observe(state(v))
      }
    }
  }


  /**
   * this detaches a factor from its current nodes and then reattaches to the nodes corresponding
   * to the potentials variables. Crucially, if the potential variables have changed
   * since the last time the factor was attached, this connects the factor to new nodes.
   * @param factor the factor to reattach.
   */
  protected def reattachFactor(factor:FactorType): Unit = {
    for ((e,v) <- factor.discEdges.iterator zip factor.pot.discVars.iterator) {
      e.node.buffer = e.node.buffer.filterNot(_ == e)
      e.node.build()
      val newNode = var2DiscNode(v)
      e.node = newNode
      e.node.buffer ::= e
      e.node.build()
    }
    for ((e,v) <- factor.contEdges.iterator zip factor.pot.contVars.iterator) {
      e.node.buffer = e.node.buffer.filterNot(_ == e)
      e.node.build()
      val newNode = var2ContNode(v)
      e.node = newNode
      e.node.buffer ::= e
      e.node.build()
    }
    for ((e,v) <- factor.vectEdges.iterator zip factor.pot.vectVars.iterator) {
      e.node.buffer = e.node.buffer.filterNot(_ == e)
      e.node.build()
      val newNode = var2VectNode(v)
      e.node = newNode
      e.node.buffer ::= e
      e.node.build()
    }
  }

  //---- These are hacks to make Intellij be able to parse this file, there are much nicer ways to do this otherwise
  private def createDiscEdges(factor: FactorType, linkToNormalFactors: Boolean = true): Unit = {
    val result = new ArrayBuffer[DiscEdge]
    for ((v, i) <- factor.pot.discVars.iterator.zipWithIndex) result += createDiscEdge(var2DiscNode(v), factor, i)
    factor.discEdges = result.toArray[DiscEdge](discEdgeTag)
    if (linkToNormalFactors) factor.discEdges.foreach(e => e.node.buffer ::= e)
    else factor.discEdges.foreach(e => e.node.statsEdgeBuffer ::= e)
  }

  private def createContEdges(factor: FactorType, linkToNormalFactors: Boolean = true): Unit = {
    val result = new ArrayBuffer[ContEdge]
    for ((v, i) <- factor.pot.contVars.iterator.zipWithIndex) result += createContEdge(var2ContNode(v), factor, i)
    factor.contEdges = result.toArray[ContEdge](contEdgeTag)
    if (linkToNormalFactors) factor.contEdges.foreach(e => e.node.buffer ::= e)
    else factor.contEdges.foreach(e => e.node.statsEdgeBuffer ::= e)
  }

  private def createVectEdges(factor: FactorType, linkToNormalFactors: Boolean = true): Unit = {
    val result = new ArrayBuffer[VectEdge]
    for ((v, i) <- factor.pot.vectVars.iterator.zipWithIndex) result += createVectEdge(var2VectNode(v), factor, i)
    factor.vectEdges = result.toArray[VectEdge](vectEdgeTag)
    if (linkToNormalFactors) factor.vectEdges.foreach(e => e.node.buffer ::= e)
    else factor.vectEdges.foreach(e => e.node.statsEdgeBuffer ::= e)
  }


  private def createDiscNodes() = {
    val map = new mutable.HashMap[DiscVar[Any], DiscNode]
    for (v <- problem.discVars) map(v) = createDiscNode(v)
    Map() ++ map
  }

  private def createContNodes() = {
    val map = new mutable.HashMap[ContVar, ContNode]
    for (v <- problem.contVars) map(v) = createContNode(v)
    Map() ++ map
  }

  private def createVectNodes() = {
    val map = new mutable.HashMap[VectVar, VectNode]
    for (v <- problem.vectVars) map(v) = createVectNode(v)
    Map() ++ map
  }


  //class tags allow us to create edge arrays without know the concrete array element class yet.
  implicit def discEdgeTag: ClassTag[DiscEdge]
  implicit def contEdgeTag: ClassTag[ContEdge]
  implicit def vectEdgeTag: ClassTag[VectEdge]
  def observationChanged(obs: State) = {
    setObservations(obs)
  }

  val var2DiscNode = createDiscNodes()
  val var2ContNode = createContNodes()
  val var2VectNode = createVectNodes()

  val factors      = problem.pots.map(p => createAndLinkFactor(p))
  val pot2Factor   = factors.map(f => f.pot -> f).toMap
  val statsFactors = problem.stats.map(p => createAndLinkFactor(p, false))


  var2DiscNode.values.foreach(_.build())
  var2ContNode.values.foreach(_.build())
  var2VectNode.values.foreach(_.build())

  setObservations()

  problem.listeners += this

}
