package ml.wolfe.nlp

/**
 * Created by narad on 12/5/14.
 */
/**
 * A sparse dependency tree.  Not all arcs require a head.
 * @param arcs tuples of child, head, and label fields for each token with a head.
 */
case class DependencyTree(tokens: IndexedSeq[Token], arcs: Seq[(Int,Int,String)]) {

  def childrenOf(i: Int): Seq[Int] = {
    arcs.filter(_._2 == i).map(_._1)
  }

  def shortestDirectedPath(source: Int, dest: Int, max: Int = size): Option[Seq[Int]] = {
    if (arcs.exists(a => a._2 == source && a._1 == dest)) {
      Some(Seq(source, dest))
    }
    else if (max == 0) {
      None
    }
    else {
      val subpaths = childrenOf(source).map(shortestDirectedPath(_, dest, max - 1)).flatten
      if (subpaths.isEmpty)
        None
      else
        Some(Seq(source) ++ subpaths.sortBy(_.size).head)
    }
  }

  def crosses(a1: (Int,Int,String), a2: (Int,Int,String)): Boolean = crosses(a1._1, a1._2, a2._1, a2._2)

  def crosses(ii: Int, ij: Int, ik: Int, il: Int): Boolean = {
    val (i,j) = if (ii < ij) (ii, ij) else (ij, ii)
    val (k,l) = if (ik < il) (ik, il) else (il, ik)
    (i < k && k < j && j < l) || (k < i && j < k && l < j)
  }

  def hasHead(i: Int, j: Int) = arcs.exists(n => n._1 == i && n._2 == j)

  def headOf(i: Int) = arcs.find(_._1 == i) match {
    case Some(x) => Some(x._2)
    case _ => None
  }

  def labelOf(i: Int) = arcs.find(_._1 == i) match {
    case Some(x) => Some(x._3)
    case _ => None
  }

  def isProjective = !arcs.exists(a1 => arcs.exists(a2 => crosses(a1,a2)))

  override def toString = arcs.mkString("\n")

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
    val arcs = Seq((2,3), (1,2), (0,1), (4, 3), (5,4), (6,5)).map{p => (p._1, p._2, "blah")}
    val tree = DependencyTree(tokens, arcs)
    println(tree.toString)
    println("SDP: " + tree.shortestDirectedPath(3,0))
    println(tree.shortestDirectedPath(6,1))
  }
}
