package ml.wolfe.nlp.syntax

import ml.wolfe.nlp.{CharOffsets, Token}

/**
 * Created by narad on 12/5/14.
 */


/**
 * A container class for the arcs in a DependencyTree
 * @param parent index of the parent (source) of the arc in the sentence.
 * @param child index of the child (destination) of the arc in the sentence.
 * @param label label of syntactic role expressed by the arc
 */
case class Arc(parent: Int, child: Int, label: Option[String] = None)


/**
 * A sparse dependency tree.  Not all tokens require a head.
 * @param tokens tokens of the sentence.
 * @param arcs arcs of the DependencyTree.
 */
case class DependencyTree(tokens: IndexedSeq[Token], arcs: Seq[Arc]) {

  def childOf(i: Int, j: Int): Boolean = arcs.exists(a => a.child == i && a.parent == j)

  def parentOf(i: Int, j: Int): Boolean = arcs.exists(a => a.child == j && a.parent == i)

  def childrenOf(i: Int): Seq[Int] = {
    arcs.filter(_.parent == i).map(_.child)
  }

  // Should only be one, but for safety and symmetry
  def parentsOf(i: Int): Seq[Int] = {
    arcs.filter(_.child == i).map(_.parent)
  }

  def shortestPath(source: Int, dest: Int, visited: Seq[Int] = Seq(), max: Int = size): Option[Seq[(Int, String, Int)]] = {
    if (max == 0) return None
    if (childOf(source, dest)) return Some(Seq((source, "CHILD", dest)))
    if (childOf(dest, source)) return Some(Seq((source, "PARENT", dest)))
    val ccands = childrenOf(source).filter(c => !visited.contains(c)).map(c => (source, "CHILD", c))
    val pcands = parentsOf(source).filter(c => !visited.contains(c)).map(c => (source, "PARENT", c))
    val paths = (ccands ++ pcands).map { t =>
      val c = t._3
      val path = shortestPath(c, dest, visited :+ source, max = size-1)
      path match {
        case None => None
        case Some(p) => Some(Seq(t) ++ p)
      }
    }.flatten
    if (paths.nonEmpty) Some(paths.sortBy(_.size).head) else None
  }

  def shortestDirectedPath(source: Int, dest: Int, max: Int = size): Option[Seq[Int]] = {
    if (max == 0) return None
    if (arcs.exists(a => a.parent == source && a.child == dest)) {
      Some(Seq(source, dest))
    }
    else {
      val subpaths = childrenOf(source).map(shortestDirectedPath(_, dest, max - 1)).flatten
      if (subpaths.isEmpty)
        None
      else
        Some(Seq(source) ++ subpaths.sortBy(_.size).head)
    }
  }

  def pathToLexicalizedString(list: Seq[(Int, String, Int)]): String = {
    list.zipWithIndex.map { case (l, i) =>
      val arrow = if (l._2 == "CHILD") "<--" else "-->"
      if (i < list.size - 1) tokens(l._1).word + arrow
      else tokens(l._3).word
    }.mkString("")
  }

  def pathToPostagString(list: Seq[(Int, String, Int)]): String = {
    list.zipWithIndex.map { case (l, i) =>
      val arrow = if (l._2 == "CHILD") "<--" else "-->"
      if (i < list.size - 1) tokens(l._1).word + arrow
      else tokens(l._3).word
    }.mkString("")
  }

  def root: Int = {
    (0 until size).find(i => arcs.exists(_.child == i)).get
  }

  def crosses(a1: Arc, a2: Arc): Boolean = {
    crosses(a1.child, a1.parent, a2.child, a2.parent)
  }

  def crosses(ii: Int, ij: Int, ik: Int, il: Int): Boolean = {
    val (i,j) = if (ii < ij) (ii, ij) else (ij, ii)
    val (k,l) = if (ik < il) (ik, il) else (il, ik)
    (i < k && k < j && j < l) || (k < i && j < k && l < j)
  }

  def hasHead(i: Int, j: Int) = arcs.exists(n => n.child == i && n.parent == j)

  def headOf(i: Int) = arcs.find(_.child == i) match {
    case Some(x) => Some(x.parent)
    case _ => None
  }

  def labelOf(i: Int) = arcs.find(_.child == i) match {
    case Some(x) => x.label
    case _ => None
  }

  def isProjective = !arcs.exists(a1 => arcs.exists(a2 => crosses(a1,a2)))

  def isLabeled = arcs.forall(_.label.isDefined)

  override def toString = {
    arcs.map { a =>
      if (a.label.isDefined) {
        tokens(a.parent).word + " -- " + a.label.get + " --> " + tokens(a.child).word
      }
      else {
        tokens(a.parent).word + " ---> " + tokens(a.child).word
      }
    }.mkString("\n")
  }

  def size = tokens.size
}

/**
 * Companion object for the DependencyTree class.
 */
object DependencyTree {
  val empty = DependencyTree(tokens = IndexedSeq(), arcs = Seq())

  def main(args: Array[String]) {
    val tokens = IndexedSeq(Token(word = "one", offsets = CharOffsets(0,1)),
                            Token(word = "two", offsets = CharOffsets(1,2)),
                            Token(word = "three", offsets = CharOffsets(2,3)),
                            Token(word = "four", offsets = CharOffsets(3,4)),
                            Token(word = "five", offsets = CharOffsets(4,5)),
                            Token(word = "six", offsets = CharOffsets(5,6)))
    val arcs = Seq(Arc(2,3), Arc(1,2), Arc(0,1), Arc(4, 3), Arc(5,4), Arc(6,5))
    val tree = DependencyTree(tokens, arcs)
    println(tree.toString)
    println("SDP: " + tree.shortestDirectedPath(3,0))
    println(tree.shortestDirectedPath(6,1))
  }
}
