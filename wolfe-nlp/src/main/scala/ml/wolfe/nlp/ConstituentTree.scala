package ml.wolfe.nlp

import ml.wolfe.nlp.io.ConstituentTreeFactory

import scala.collection.mutable.ArrayBuffer

/**
 * Created by narad on 12/5/14.
 */
/**
 * A constituent tree.
 */

case class ConstituentTree(node: ConstituentNode, children : List[ConstituentTree] = List()) {

  def label: String = node.label

  def isPreterminal = node.isPreterminal

  def isNonterminal = node.isNonterminal

  def labelsOfSpan(i: Int, j: Int): Iterator[String] = {
    index(i)(j).map(_.label).iterator
  }

  def coarsenLabels: ConstituentTree = {
    val coarsed = coarsen(node.label)
    if (node.label == coarsed) {
      new ConstituentTree(node, children.map(_.coarsenLabels))
    }
    else {
      if (isNonterminal) {
        new ConstituentTree(new NonterminalNode(coarsed), children.map(_.coarsenLabels))
      }
      else {
        new ConstituentTree(new PreterminalNode(coarsed, node.asInstanceOf[PreterminalNode].word), children.map(_.coarsenLabels))
      }
    }
  }


  def coarsen(ll: String): String = {
    if (ll == "-DFL-") return ll
    var l = ll
    while (l.startsWith("^")) {
      l = l.substring(1)
    }
    if (l.contains("|"))
      l = l.substring(0, l.indexOf("|"))
    if (l.contains("-"))
      l = l.substring(0, l.indexOf("-"))
    if (l.contains("="))
      l = l.substring(0, l.indexOf("="))
    if (l.contains("^"))
      l = l.substring(0, l.indexOf("^"))
    return l
  }

  def tags: Iterator[String] = leaves.collect { case l: PreterminalNode => l.label }

  def words: Iterator[String] = leaves.collect { case l: PreterminalNode => l.word }

  def tokens: Iterator[Token] = leaves.collect { case l: PreterminalNode => Token(word = l.word, offsets = null, posTag = l.label) }

  def setYield(words: Array[String], tags: Array[String], offset: Int = 0): ConstituentTree = {
    //    println("setting yield")
    var tally = 0
    node match {
      case x: NonterminalNode => {
        //      println(" set nonterm")
        new ConstituentTree(node, children.map{ c =>
          val child = c.setYield(words, tags, offset + tally)
          tally += c.width
          child
        })
      }
      case x: PreterminalNode => {
        //       println("offset = " + offset)
        new ConstituentTree(new PreterminalNode(tags(offset), words(offset)))
      }
    }
  }

  def index: Array[Array[ArrayBuffer[Span]]] = {
    val ispans = Array.fill(length+1,length+1)(new ArrayBuffer[Span])
    var numLeaves = 0
    for (t <- leafFirstSearch) {
      if (t.isLeaf) {
        ispans(numLeaves)(numLeaves+1) += new Span(numLeaves, numLeaves+1, t.label, 0)
        numLeaves += 1
      }
      else {
        val len = t.length
        val height = ispans(numLeaves-len)(numLeaves).size
        ispans(numLeaves-len)(numLeaves) += new Span(numLeaves-len, numLeaves, t.label, height)
      }
    }
    ispans
  }

  override def toString: String = {
    node match {
      case x: NonterminalNode => "(%s %s)".format(x.label, children.map(_.toString()).mkString(" "))
      case x: PreterminalNode => "(%s %s)".format(x.label, x.word)
      case _ => "empty"
    }
  }

  def slice(i: Int, j: Int): ConstituentTree = {
    val ospans = toSpans.toArray
    val fspans = ospans.filter{ s => s.start >= i && s.end <= j && s.width > 1} // && (span.end-span.start > 1 || span.isUnary)}
    val ss2 = fspans.map{span => Span(span.start-i, span.end-i, span.label, span.height)}
    val t = ConstituentTreeFactory.constructFromSpans(ss2, j-i, words.slice(i, j).toArray, tags.slice(i, j).toArray)
    t
  }

  def binarize(mode: String = "RIGHT_0MARKOV"): ConstituentTree = {
    //   println("-- " + children.map(_.label).mkString(", "))
    if (children.size > 2) {
      //val grandchildren = children.slice(1, children.size)
      mode match {
        case "RIGHT_0MARKOV" => {
          println("right 0 markov")
          val blabel = if (node.label.startsWith("@")) node.label else "@%s".format(node.label)
          return new ConstituentTree(node, List[ConstituentTree](
            children.head.binarize(mode),
            new ConstituentTree(new NonterminalNode(blabel), children.slice(1, children.size)).binarize(mode)))
        }
        case "LEFT_0MARKOV" => {
          println("left 0 markov")
          val blabel = if (node.label.startsWith("@")) node.label else "@%s".format(node.label)
          return new ConstituentTree(node, List[ConstituentTree](
            new ConstituentTree(new NonterminalNode(blabel), children.slice(0, children.size-1)).binarize(mode),
            children.last.binarize(mode)))
        }
        case "RIGHT_SINGLE" => {
          println("right single")
          return new ConstituentTree(node, List[ConstituentTree](
            children(0).binarize(mode),
            new ConstituentTree(new NonterminalNode("@"), children.slice(1, children.size)).binarize(mode)))
        }
        case "LEFT_SINGLE" => {
          println("left single")
          return new ConstituentTree(node, List[ConstituentTree](
            new ConstituentTree(new NonterminalNode("@"), children.slice(0, children.size-1)).binarize(mode),
            children.last.binarize(mode)))
        }
      }
    }
    else{
      return new ConstituentTree(node, children.map(_.binarize(mode)))
    }
  }

  def isBinarized: Boolean = node.label.contains("@")

  def removeUnaryChains(): ConstituentTree = {
    return new ConstituentTree(node,
      if (children.size == 1) {
        val uh = unaryHelper()
        unaryHelper().map(_.removeUnaryChains())
      }
      else {
        children.map(_.removeUnaryChains())
      })
  }

  def unaryHelper(): List[ConstituentTree] = {
    if (children.size == 0) {
      return List(this)
    }
    if (children.size == 1) {
      children(0).unaryHelper()
    }
    else {
      return children
    }
  }

  def removeNones(): ConstituentTree = {
    val nchildren = children.map(_.removeNones()).filter(_ != null.asInstanceOf[ConstituentTree])
    if (label == "-NONE-" || label == "-RRB-" || label == "-LRB-" || (children.size > 0 && nchildren.size == 0)) {
      return null.asInstanceOf[ConstituentTree]
    }
    else {
      return new ConstituentTree(node, nchildren)
    }
  }

  def removeTop: ConstituentTree = {
    assert(children.size == 1, "Attempted to remove top node on a tree that would become multi-rooted.\n" + toString)
    children(0)
  }

  def nodemap(f: (ConstituentNode) => ConstituentNode): ConstituentTree = {
    return new ConstituentTree(f(node), children.map(_.nodemap(f)))
  }

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

  def width: Int = {
    leaves.size
  }

  // Indexing Methods
  lazy private val spans = index

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

/**
 * Companion object for the ConstituentTree class.
 */
object ConstituentTree {
  val empty = ConstituentTree(node=null, children=List())
}