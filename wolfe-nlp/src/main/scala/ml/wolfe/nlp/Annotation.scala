package ml.wolfe.nlp

import scala.collection.mutable.ArrayBuffer

/**
 * A mention of a named entity.
 * @param entityMentions sequence of entity mentions found in the sentence.
 * @param relationMentions sequence of relation mentions found in the sentence.
 * @param eventMentions sequence of event mentions found in the sentence.
 */
case class IEAnnotation(entityMentions: Seq[EntityMention],
                        relationMentions: Seq[RelationMention],
                        eventMentions: Seq[EventMention]) {}

/**
 * Companion object for the IEAnnotation class.
 */
object IEAnnotation {
  val empty = IEAnnotation(Seq.empty,Seq.empty,Seq.empty)
}

/**
 * A mention of a named entity.
 * @param label label of the entity.
 * @param start index to the token that begins the entity span.
 * @param end index to the token that ends the entity span.
 * @param id mention-specific identifier of the entity span.
 */
case class EntityMention(label: String, start: Int, end: Int, id: String = null) {}

/**
 * A directed relation mention.
 * @param label label of the relation.
 * @param arg1 index into sentence.entities() for the first argument (parent of the relation)
 * @param arg2 index into sentence.entities() for the second argument (child of the relation)
 */
case class RelationMention(label: String, arg1: Int, arg2: Int, id: String = null) {}

/**
 * An event mention.
 * @param label label of the event.
 * @param trigger trigger word for the event.
 * @param arguments a sequence of argument roles.
 */
case class EventMention(label: String, trigger: EntityMention, arguments: Seq[RoleMention], id: String = null) {}

/**
 * A role mention.
 * @param label label of the role.
 * @param arg the target of the role.
 */
case class RoleMention(label: String, arg: EntityMention) {}







/**
 * A container for syntactic annotation.
 * @param tree constituent tree for the sentence.
 * @param dependencies dependency tree for the sentence.
 */
case class SyntaxAnnotation(tree: ConstituentTree, dependencies: DependencyTree) {}

/**
 * Companion object for the SyntaxAnnotation class.
 */
object SyntaxAnnotation {
  val empty = SyntaxAnnotation(tree = ConstituentTree.empty, dependencies = DependencyTree.empty)
}

/**
 * A sparse dependency tree.  Not all nodes require a head.
 * @param nodes tuples of child, head, and label fields for each token with a head.
 */
case class DependencyTree(nodes: Seq[(Int,Int,String)]) {
  def hasHead(i: Int, j: Int) = nodes.exists(n => n._1 == i && n._2 == j)
  override def toString = nodes.mkString("\n")
}

/**
 * Companion object for the DependencyTree class.
 */
object DependencyTree {
  val empty = DependencyTree(nodes = Seq())
}

/**
 * A constituent tree.
 */
//case class ConstituentTree() {}

/**
 * Companion object for the ConstituentTree class.
 */
object ConstituentTree {
  val empty = ConstituentTree(node=null, children=Seq())
}


case class ConstituentTree(node: ConstituentNode, children : Seq[ConstituentTree] = Seq()) {

  private lazy val len = leaves.size

  def breadthFirstSearch: Iterator[ConstituentTree] = {
    Iterator.single(this) ++ children.iterator ++ children.flatMap(_.breadthFirstSeachHelper)
  }

  private def breadthFirstSeachHelper: Iterator[ConstituentTree] = {
    children.iterator ++ children.flatMap(_.breadthFirstSeachHelper)
  }

  def depthFirstSearch: Iterator[ConstituentTree] = {
    Iterator.single(this) ++ children.flatMap(_.depthFirstSearch)
  }

  def leafFirstSearch: Iterator[ConstituentTree] = {
    children.flatMap(_.leafFirstSearch).iterator ++ Iterator.single(this)
  }

  def height: Int = {
    if (isLeaf) {
      0
    }
    else {
      children.map(_.height).max + 1
    }
  }

  def isLeaf: Boolean = {
    children.size == 0
  }

  def leaves: Iterator[ConstituentNode] = {
    depthFirstSearch.collect { case x: ConstituentTree if x.isLeaf => x.node }
  }

  def length: Int = len

  override def toString(): String = {
    if (isLeaf) {
      return "(%s)".format(node.toString())
    }
    else {
      return "(%s %s)".format(node.toString(), children.map(_.toString()).mkString(" "))
    }
  }

  def width: Int = {
    leaves.size
  }

  // Indexing Methods
  lazy private val spans = index

  def index: Array[Array[ArrayBuffer[Span]]] = {
    val ispans = Array.fill(length+1,length+1)(new ArrayBuffer[Span])
    var numLeaves = 0
    for (t <- leafFirstSearch) {
      if (t.isLeaf) {
        ispans(numLeaves)(numLeaves+1) += new Span(numLeaves, numLeaves+1, node.toString, 0)
        numLeaves += 1
      }
      else {
        val len = t.length
        val height = ispans(numLeaves-len)(numLeaves).size
        ispans(numLeaves-len)(numLeaves) += new Span(numLeaves-len, numLeaves, node.toString, height)
      }
    }
    ispans
  }

  def containsSpan(i: Int, j: Int): Boolean = {
    if (i < 0 || j < 0) return false
    if (i > length || j > length) return false
    !spans(i)(j).isEmpty
  }

  def containsSpan(i: Int, j: Int, l: String): Boolean = {
    if (!containsSpan(i, j)) return false
    return spans(i)(j).exists(_.label == l)
  }

  def containsUnarySpan(i: Int, j: Int): Boolean = {
    if (i < 0 || j < 0) return false
    if (i > length || j > length) return false
    spans(i)(j).exists(_.isUnary)
  }

  def containsUnarySpan(i: Int, j: Int, l: String): Boolean = {
    if (i < 0 || j < 0) return false
    if (i > length || j > length) return false
    spans(i)(j).exists(s => s.isUnary && s.label == l)
  }

  def containsUnarySpan(i: Int, j: Int, l: String, h: Int): Boolean = {
    if (i < 0 || j < 0) return false
    if (i > length || j > length) return false
    spans(i)(j).exists(s => s.isUnary && s.label == l && s.height == h)
  }

  def containsLabel(i: Int, j: Int, l: String): Boolean = {
    if (i < 0 || j < 0) return false
    if (i > length || j > length) return false
    spans(i)(j).exists(s => s.label == l)
  }

  def highestUnarySpan(i: Int, j: Int): String = {
    if (i < 0 || j < 0) return "none"
    if (i > length || j > length) return "none"
    if (spans(i)(j).filter(_.isUnary).size > 0) {
      spans(i)(j).filter(_.isUnary).sortBy(_.height * -1).head.label
    }
    else {
      "none"
    }
  }

  def toSpans: Iterable[Span] = {
    for (i <- 0 until length; j <- 1 to length; k <- 0 until spans(i)(j).size) yield spans(i)(j)(k)
  }

  def spansAt(i: Int, j: Int): Iterable[Span] = spans(i)(j).toIterable
}

abstract class ConstituentNode(val label: String) {

  def isNonterminal: Boolean = this match {
    case x: NonterminalNode => true
    case _ => false
  }

  def isPreterminal: Boolean = this match {
    case x: PreterminalNode => true
    case _ => false
  }
}

case class NonterminalNode(override val label: String, head: Int = -1) extends ConstituentNode(label) {

  override def isNonterminal = true

  override def isPreterminal = false
}

case class PreterminalNode(override val label: String, word: String) extends ConstituentNode(label) {

  override def isNonterminal = false

  override def isPreterminal = true
}

case class Span(left: Int, right: Int, label: String, var height: Int=0) {

  def width: Int = right - left

  def covers(other: Span): Boolean = {
    return left <= other.left &&
    right >= other.right &&
    !equals(other)
  }

  def crosses(other: Span): Boolean = {
    return (start < other.start && end > other.start   && end < other.end) ||
    (start > other.start && start < other.end && end > other.end)
  }

  override def equals(that: Any): Boolean = that match {
    case other: Span => {
      left == other.left && right == other.right && other.label == label
    }
    case _=> false
  }

  def isUnary = height > 0

  def start = left

  def end = right

  def isTerminal = !isUnary && width == 1

  override def toString(): String = "%s(%s,%s,%d)".format(label, left, right, height)
}