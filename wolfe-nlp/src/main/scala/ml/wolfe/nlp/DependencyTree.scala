package ml.wolfe.nlp

/**
 * Created by narad on 12/5/14.
 */
/**
 * A sparse dependency tree.  Not all arcs require a head.
 * @param arcs tuples of child, head, and label fields for each token with a head.
 */
case class DependencyTree(tokens: IndexedSeq[Token], arcs: IndexedSeq[(Int,Int,String)]) {
  def crosses(a1: (Int,Int,String), a2: (Int,Int,String)): Boolean = crosses(a1._1, a1._2, a2._1, a2._2)

  def crosses(ii: Int, ij: Int, ik: Int, il: Int): Boolean = {
    val (i,j) = if (ii < ij) (ii, ij) else (ij, ii)
    val (k,l) = if (ik < il) (ik, il) else (il, ik)
    (i < k && k < j && j < l) || (k < i && j < k && l < j)
  }

  def hasHead(i: Int, j: Int) = arcs.exists(n => n._1 == i && n._2 == j)

  def headOf(j: Int) = arcs.find(_._1 == j) match {
    case Some(x) => Some(x._2)
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
  val empty = DependencyTree(tokens = IndexedSeq(), arcs = IndexedSeq())
}
