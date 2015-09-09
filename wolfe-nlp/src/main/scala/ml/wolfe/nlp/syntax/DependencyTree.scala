package ml.wolfe.nlp.syntax

import ml.wolfe.nlp.{CharOffsets, Token}

/**
 * Created by narad on 12/5/14.
 */


/**
 * A container class for the arcs in a DependencyTree
 * @param parent index of the parent (head / source) of the arc in the sentence.
 * @param child index of the child (modifier / target) of the arc in the sentence.
 * @param label label of syntactic role expressed by the arc
 */
case class Arc(parent: Int, child: Int, label: Option[String] = None) {
  override def toString = "(" + parent + " -> " + child + ", " + label + ")"
}


/**
 * A sparse dependency tree.  Not all tokens require a head.
 * Methods assume a 0-offset token index (i.e., does not posit the existence of a dummy root node)
 * @param tokens tokens of the sentence.
 * @param arcs arcs of the DependencyTree.
 */
case class DependencyTree(tokens: IndexedSeq[Token], arcs: Seq[Arc]) {

  def isChildOf(i: Int, j: Int): Boolean = arcs.exists(a => a.child == i && a.parent == j)

  def isParentOf(i: Int, j: Int): Boolean = arcs.exists(a => a.child == j && a.parent == i)

  def childrenOf(i: Int): Seq[Int] = {
    arcs.filter(_.parent == i).map(_.child)
  }

  def childArcsOf(i: Int): Seq[Arc] = {
    arcs.filter(_.parent == i)
  }

  // Should only be one, but for safety and symmetry
  def parentsOf(i: Int): Seq[Int] = {
    arcs.filter(_.child == i).map(_.parent)
  }

  def parentArcsOf(i: Int): Seq[Arc] = {
    arcs.filter(_.parent == i)
  }

  def shortestPath(source: Int, dest: Int, visited: Seq[Int] = Seq(), max: Int = size): Option[Seq[PathArc]] = {
    if (max == 0) return None
    arcs.find(a => a.parent == source && a.child == dest) match {
      case Some(arc) => return Some(Seq(PathArc(source, dest, arc.parent, arc.child, arc.label)))
      case None => {}
    }
    arcs.find(a => a.parent == dest && a.child == source) match {
      case Some(arc) => return Some(Seq(PathArc(source, dest, arc.child, arc.parent, arc.label)))
      case None => {}
    }

    val childPaths = arcs.filter(a => source == a.parent && !visited.contains(a.child)).map { a =>
      val path = shortestPath(a.child, dest, visited :+ source, max = size-1)
      path match {
        case None => None
        case Some(p) => Some(Seq(PathArc(source, a.child, parent = a.parent, child = a.child, label = a.label)) ++ p)
      }
    }
    val parentPaths = arcs.filter(a => source == a.child && !visited.contains(a.parent)).map { a =>
      val path = shortestPath(a.parent, dest, visited :+ source, max = size-1)
      path match {
        case None => None
        case Some(p) => Some(Seq(PathArc(source, a.parent, parent = a.parent, child = a.child, label = a.label)) ++ p)
      }
    }
    val paths = (childPaths ++ parentPaths).flatten
    if (paths.isEmpty)
      return None
    else
      Some(paths.minBy(_.size))
  }

  def shortestDirectedPath(source: Int, dest: Int, max: Int = size): Option[Seq[PathArc]] = {
    if (max == 0) return None
    arcs.find(a => a.parent == source && a.child == dest) match {
      case Some(arc) => return Some(Seq(PathArc(source, dest, arc.parent, arc.child, arc.label)))
      case None => {}
    }
    val subpaths = childrenOf(source).map(shortestDirectedPath(_, dest, max - 1)).flatten
    if (subpaths.isEmpty) {
      None
    }
    else {
      val shortestSubpath = subpaths.minBy(_.size)
      val pivot = arcs.find(a => a.parent == source && a.child == shortestSubpath.head.parent)
      pivot match {
        case Some(arc) => Some(Seq(PathArc(source, dest, arc.parent, arc.child, arc.label)) ++ shortestSubpath)
      }
    }
  }

  def root: Int = {
    (0 until size).filter(child => !arcs.exists(_.child == child)).head
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
    def word(index:Int) = tokens.lift(index).map(_.word).getOrElse(index.toString)
    arcs.map { a =>
      if (a.label.isDefined) {
        word(a.parent) + " -- " + a.label.get + " --> " + word(a.child)
      }
      else {
        word(a.parent) + " --> " + word(a.child)
      }
    }.mkString("\n")
  }

  def size = tokens.size
}

case class PathArc(source: Int, dest: Int, parent: Int, child: Int, label: Option[String]) {

  val direction: String = {
    if (source < dest) "->" else "<-"
  }
}

/**
 * Companion object for the DependencyTree class.
 */
object DependencyTree {
  val empty = DependencyTree(tokens = IndexedSeq(), arcs = Seq())
}












/*


//      !visited.contains(a.parent) && !visited.contains((a.child))).map {a =>

//    }

    val ccands = childrenOf(source).filter(c => !visited.contains(c)).map(c => PathArc(source, c, source, c, ))// (source, "CHILD", c))
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

  def shortestPath(source: Int, dest: Int, visited: Seq[Int] = Seq(), max: Int = size): Option[Seq[(Int, String, Int)]] = {
    if (max == 0) return None
    if (childOf(source, dest)) return Some(Seq((source, "PARENT", dest)))
    if (childOf(dest, source)) return Some(Seq((source, "CHILD", dest)))
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

  def shortestDirectedPath2(source: Int, dest: Int, max: Int = size): Option[Seq[Arc]] = {
    if (max == 0) return None
    arcs.find(a => a.parent == source && a.child == dest) match {
      case Some(arc) => return Some(Seq(arc))
      case None => {}
    }
    val subpaths = childrenOf(source).map(shortestDirectedPath2(_, dest, max - 1)).flatten
    if (subpaths.isEmpty) {
      None
    }
    else {
      val shortestSubpath = subpaths.minBy(_.size)
      val pivot = arcs.find(a => a.parent == source && a.child == shortestSubpath.head.parent)
      pivot match {
        case Some(arc) => Some(Seq(arc) ++ shortestSubpath)
      }
    }
  }

 */