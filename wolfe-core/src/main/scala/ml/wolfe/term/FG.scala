package ml.wolfe.term

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

/**
 * @author riedel
 */
class FG[NodeContent, EdgeContent, FactorContent] {

  class Node(val variable: AnyGroundAtom, val content: NodeContent) {
    val edges = new ArrayBuffer[Edge]
  }

  class Pointer(val source:AnyVar, grounder:AnyVar => AnyGroundAtom) {
    def target:AnyGroundAtom = grounder(source)
    var node:Node = null
    def updateNode(): Unit = {
      node = nodes(target)
    }
  }

  class Edge(val dynVar: Pointer, val factor: Factor, val content: EdgeContent) {
    def node = dynVar.node
    def updateNode() = dynVar.updateNode()
  }

  class Factor(val potential: DoubleTerm, val content: FactorContent) {
    val edges = new ArrayBuffer[Edge]
  }

  val nodes = new mutable.LinkedHashMap[AnyGroundAtom, Node]
  val factors = new mutable.LinkedHashMap[DoubleTerm, Factor]
  val edges = new ArrayBuffer[Edge]()

  val activeFactors = new mutable.LinkedHashSet[Factor]
  val activeEdges = new mutable.LinkedHashSet[Edge]
  val activeNodes = new mutable.LinkedHashSet[Node]


  def addNode(variable: AnyGroundAtom, content: NodeContent): Node = {
    val node = new Node(variable, content)
    nodes(variable) = node
    node
  }


  def addFactor(potential: DoubleTerm, content: FactorContent, edgeFilter:AnyVar => Boolean,
                dyn: AnyVar => AnyGroundAtom)(edgeContent: AnyVar => EdgeContent): Factor = {
    val factor = new Factor(potential, content)
    for (v <- potential.vars if edgeFilter(v)) {
      val n = new Pointer(v,dyn)
      val edge = new Edge(n, factor, edgeContent(v))
      edges += edge
      factor.edges += edge
    }
    factors(potential) = factor
    factor
  }


  def deactivate(): Unit = {
    for (n <- activeNodes) n.edges.clear()
    activeNodes.clear()
    activeEdges.clear()
    activeFactors.clear()
  }

  private def deactivate(f: Factor): Unit = {
    for (e <- f.edges) {
      e.node.edges -= e
      activeEdges -= e
      if (e.node.edges.isEmpty) activeNodes -= e.node
    }
    activeFactors -= f
  }


  def activate(factor: Factor): Unit = {
    for (e <- factor.edges) {
      e.updateNode()
      e.node.edges += e
      activeEdges += e
      activeNodes += e.node
    }
    activeFactors += factor
  }

  case class DirectedEdge(edge:Edge, isF2N:Boolean) {
    def flip = DirectedEdge(edge, !isF2N)
    override def toString = if(isF2N) {
      "DirectedEdge(" + edge.factor + " -> " + edge.node + ")"
    } else {
      "DirectedEdge(" + edge.node + " -> " + edge.factor + ")"
    }
  }

  def scheduleInward(): Seq[DirectedEdge] = {
    @tailrec
    def scheduleAcc(queue:Seq[Node], done: Seq[Node], acc: Seq[DirectedEdge]): Seq[DirectedEdge] = {
      queue match {
        case Nil =>
          val unseen = activeNodes.find(! done.contains(_))
          unseen match {
            case None => acc
            case Some(n) => scheduleAcc(Seq(n), done, acc)
          }

        case head :: tail =>
          if (done.contains(head)) {
            scheduleAcc(tail, done, acc)
          } else {
            val newEdges = head.edges.filter(e => !acc.exists(_.edge == e) && activeEdges.contains(e)).flatMap{ e1 =>
              e1.factor.edges.filter(e => e != e1 && activeEdges.contains(e)).map{ e2 =>
                DirectedEdge(e2, isF2N = false)
              } :+ DirectedEdge(e1, isF2N = true)
            }
            scheduleAcc(tail, head +: done, newEdges ++ acc)
          }
      }
    }
    scheduleAcc(Seq(), Seq(), Seq())
  }

  def scheduleInwardOutward(): Seq[DirectedEdge] = {
    val inward = scheduleInward()
    val outward = inward.reverseMap(_.flip)
    inward ++ outward
  }

  def scheduleOutward(): Seq[DirectedEdge] = scheduleInward().reverseMap(_.flip)

  case class MessageRecord(edge: Edge, direction:String, message:Msg)
  val messageHistory:ArrayBuffer[ArrayBuffer[MessageRecord]] = ArrayBuffer()

  def recordF2N(edge:Edge, message:Msg): Unit = {
    messageHistory += ArrayBuffer(MessageRecord(edge, "F2N", message))
  }
  def recordN2F(edge:Edge, message:Msg): Unit = {
    messageHistory += ArrayBuffer(MessageRecord(edge, "N2F", message))
  }

}
