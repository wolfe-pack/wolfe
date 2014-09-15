package ml.wolfe.fg

import ml.wolfe.FactorGraph.Edge
import ml.wolfe.{FactorGraph, Wolfe}
import Wolfe._

import scala.math._

/**
 * @author Sebastian Riedel
 */
class TreePotential(val edges: Map[(Any, Any), Edge], val ord: Edge, multinode: Boolean) extends Potential {
  val v = ord.n.variable
  private val ordering = ord.n.variable.asTyped[Ordering[Any]].value
  private val distinctHeads = edges.keys.map(_._1).toSeq.distinct.sorted(ordering)
  private val headIndices: Map[Any, Int] = distinctHeads.zipWithIndex.toMap
  private val indexedEdges:Map[(Int, Int), Edge] = edges.map {
    case ((x:Any, y:Any), e:Edge) => ((headIndices(x), headIndices(y)), e)
  }.toMap

  /**
   * Calculate and update the MAP messages from the factor of this potential to all indexedEdges.
   */
  override def mapF2N() = {
    val in = indexedEdges(1 -> 2).msgs.asDiscrete
    val factor2nodeMsg = in.f2n
    val node2factorMsg = in.n2f

    val trueScoreFromNode = node2factorMsg(1)
  }
  override def valueForCurrentSetting() = {
    val graph = indexedEdges mapValues (_.n.variable.asDiscrete.value == 1)
    val tree = TreePotential.isFullyConnectedProjectiveTree(graph)
    if (tree) 0.0 else Double.NegativeInfinity
  }

  override def powMarginalF2N() = {           // Projective
    val slen = distinctHeads.length - 1 // indexedEdges.keys.map(_._1).max -- should indexedEdges simply be a Map[(Int,Int)]
                  // Or calculate as n as : n * (n-1) indexedEdges ?
    val maxdim =  slen + 1
    val worksize = maxdim * maxdim
    val tkmat    = Array.ofDim[Double](worksize + maxdim)
    val gradmat  = Array.ofDim[Double](worksize + maxdim)

    val heads = Array.fill(slen)(-1)

    // Collect incoming messages and look for absolute (entirely true or false) variable beliefs
    for (dep <- 1 to slen) {
      val tkoffset = dep * slen
      tkmat(tkoffset + dep - 1) = 0
      var trues = 0
      var trueHead = -1
      for (head <- 0 to slen if dep != head) {
        val m = indexedEdges(head, dep).msgs.asDiscrete.n2f
        if (m(0) == 0) {
          trues += 1
          trueHead = head
        }
        else {
          val score = m(1) / m(0)
          tkmat(head * slen + dep - 1) = score * -1.0
          tkmat(tkoffset + dep - 1) += score
        }
      }
      if (trues == 1) {
        heads(dep-1) = trueHead
        tkmat(tkoffset + dep - 1) = 1
        for (head <- 0 to slen if dep != head) {
          tkmat(head * slen + dep - 1) = if (head == trueHead) -1 else 0
        }
      }
      else if (trues > 1) {
        heads(dep-1) = -2
      }
      else {
        heads(dep-1) = -1
      }
    }
    // Calculate the potential's log partition function
    val z = sumTree(tkmat, gradmat, slen, multirooted=false)
    // Originally had a check here for Z != 0
    // Compute outgoing messages in terms of Z and incoming messages
    for (dep <- 1 to slen) {
      val koff = (dep - 1) * slen
      val tkoff = dep * slen
      for (head <- 0 to slen if dep != head) {
        val m = heads(dep-1) match {
          case -2 =>	Array(Double.NaN, Double.NaN) // Incoming beliefs are a conflicting configuration
          case -1 =>  {                             // No absolute belief was found pertaining to this head
            val s = if (head > dep) 1 else 0
            val n = gradmat(koff + head - s)
            Array(1 + tkmat(head * slen + dep - 1) * n, n)
          }
          case _ => if (heads(dep-1) == head) Array(0.0, 1.0) else Array(1.0, 0.0) // Set outgoing message deterministically
        }
        indexedEdges(head, dep).msgs.asDiscrete.f2n(0) = m(0)
        indexedEdges(head, dep).msgs.asDiscrete.f2n(1) = m(1)
      }
    }
  }

  def sumTree(tkmat: Array[Double], gradmat: Array[Double], slen: Int, multirooted: Boolean = false, verbose: Boolean = false): Double = {
    val sch = Array.ofDim[Double](slen+1, slen+1, 2, 2)
    val gch = Array.ofDim[Double](slen+1, slen+1, 2, 2)
    var res = 0.0
    val start = if (multirooted) 0 else 1
    for (i <- 0 until slen*slen) gradmat(i) = Double.NegativeInfinity
    for (s <- 0 to slen; i <- 0 to 1; j <- 0 to 1) sch(s)(s)(i)(j) = 0.0
    for (width <- 1 to slen; s <- start to slen) {
      val t = s + width
      if (t <= slen) {
        for (i <- 0 to 1; j <- 0 to 1) sch(s)(t)(i)(j) = Double.NegativeInfinity
        if (s > 0) {
          val lkid = log(-1.0 * tkmat(t * slen + s-1))
          for (r <- s until t) {
            sch(s)(t)(0)(0) = logIncrement(sch(s)(t)(0)(0), sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1) + lkid)
          }
        }
        val rkid = Math.log(-1.0 * tkmat(s * slen + t-1))
        for (r <- s until t) sch(s)(t)(1)(0) = logIncrement(sch(s)(t)(1)(0), sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1) + rkid)
        for (r <- s until t) sch(s)(t)(0)(1) = logIncrement(sch(s)(t)(0)(1), sch(s)(r)(0)(1) + sch(r)(t)(0)(0))
        for (r <- s+1 to t) sch(s)(t)(1)(1) = logIncrement(sch(s)(t)(1)(1), sch(s)(r)(1)(0) + sch(r)(t)(1)(1))
      }
    }
    if (!multirooted) {
      sch(0)(slen)(1)(1) = Double.NegativeInfinity
      for (r <- 1 to slen) {
        sch(0)(slen)(1)(1) = logIncrement(sch(0)(slen)(1)(1), sch(1)(r)(0)(1) + sch(r)(slen)(1)(1) + Math.log(-1.0 * tkmat(r-1)))
      }
    }
    res = sch(0)(slen)(1)(1)
    for (s <- 0 to slen; t <- s to slen; i <- 0 to 1; j <- 0 to 1) {
      gch(s)(t)(i)(j) = Double.NegativeInfinity
    }
    gch(0)(slen)(1)(1) = -1.0 * res
    if (!multirooted) {
      for (r <- 1 to slen) {
        gch(1)(r)(0)(1) = logIncrement(gch(1)(r)(0)(1),
          -1.0 * res + sch(r)(slen)(1)(1) + log(-1.0 * tkmat(r-1)))
        gch(r)(slen)(1)(1) = logIncrement(gch(r)(slen)(1)(1),
          -1.0 * res + sch(1)(r)(0)(1) + log(-1.0 * tkmat(r-1)))
        gradmat((r-1) * slen) = logIncrement(gradmat((r-1) * slen),
          -1.0 * res + sch(1)(r)(0)(1) + sch(r)(slen)(1)(1))
      }
    }
    for (width <- slen to 1 by -1; s <- start to slen) {
      val t = s + width
      if (t <= slen) {
        var gpar = gch(s)(t)(1)(1)
        for (r <- s+1 to t) {
          gch(s)(r)(1)(0) = logIncrement(gch(s)(r)(1)(0), gpar + sch(r)(t)(1)(1))
          gch(r)(t)(1)(1) = logIncrement(gch(r)(t)(1)(1), gpar + sch(s)(r)(1)(0))
        }
        gpar = gch(s)(t)(0)(1)  // this seems to be s,r instead of s,t for some reason
        for (r <- s until t) {
          gch(s)(r)(0)(1) = logIncrement(gch(s)(r)(0)(1), gpar + sch(r)(t)(0)(0))
          gch(r)(t)(0)(0) = logIncrement(gch(r)(t)(0)(0), gpar + sch(s)(r)(0)(1))
        }
        if (s > 0) {
          var lgrad = Double.NegativeInfinity
          val lkid = scala.math.log(-1.0 * tkmat(t * slen + s-1))
          gpar = gch(s)(t)(0)(0)
          for (r <- s until t) {
            gch(s)(r)(1)(1) 	= logIncrement(gch(s)(r)(1)(1), gpar + sch(r+1)(t)(0)(1) + lkid)
            gch(r+1)(t)(0)(1) = logIncrement(gch(r+1)(t)(0)(1), gpar + sch(s)(r)(1)(1) + lkid)
            lgrad = logIncrement(lgrad, gpar + sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1))
          }
          gradmat((s-1) * slen + t-1) = logIncrement(gradmat((s-1) * slen + t-1), lgrad)
        }
        val rkid = Math.log(-1.0 * tkmat(s * slen + t-1))
        var rgrad = Double.NegativeInfinity
        gpar = gch(s)(t)(1)(0)
        for (r <- s until t) {
          gch(s)(r)(1)(1)   = logIncrement(gch(s)(r)(1)(1), gpar + sch(r+1)(t)(0)(1) + rkid)
          gch(r+1)(t)(0)(1) = logIncrement(gch(r+1)(t)(0)(1), gpar + sch(s)(r)(1)(1) + rkid)
          rgrad = logIncrement(rgrad, gpar + sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1))
        }
        gradmat((t-1) * slen + s) = logIncrement(gradmat((t-1) * slen + s), rgrad)
      }
    }
    for (i <- 0 until slen * slen) gradmat(i) = Math.exp(gradmat(i))
    return Math.abs(res)
  }

  def logIncrement(s: Double, x: Double): Double = {
    if (s == Double.NegativeInfinity) {
      x
    }
    else {
      val d = s - x
      if (d >= 0) {
        if (d <= 745) s + log(1.0 + exp(-1.0 * d)) else s
      }
      else if (d < -745) {
        x
      }
      else {
        x + log(1.0 + exp(d))
      }
    }
  }
}

object TreePotential {

  @Wolfe.Potential( (edges: Map[(Any, Any), FactorGraph.Edge], ord:Edge) => new TreePotential(edges, ord, true) )
  def treeConstraint[T : Ordering](tree: Map[(T, T), Boolean]) =
    if (isFullyConnectedProjectiveTree(tree)) 0.0 else Double.NegativeInfinity

  def isFullyConnectedProjectiveTree[T: Ordering](graph: Map[(T, T), Boolean]) = {
    val ord = implicitly[Ordering[T]]
    import ord._

    val indexedEdges = (graph filter (_._2)).toList map (_._1)
    val nodes = (graph.keys flatMap (p => List(p._1, p._2))).toList.distinct
    if (indexedEdges.size != nodes.size - 1) false
    else {
      val children = indexedEdges groupBy (_._1) mapValues (_ map (_._2)) withDefaultValue Nil
      val parents = indexedEdges groupBy (_._2) mapValues (_ map (_._1)) withDefaultValue Nil

      def connected(remainder: List[T], visited: Set[T] = Set.empty): Set[T] = {
        def neighbors(node: T) = (children(node) ++ parents(node)) filterNot visited
        remainder match {
          case Nil => visited
          case h :: t =>
            connected(t ::: neighbors(h), visited + h)
        }
      }

      val root = nodes.min
      val isTree = connected(nodes.head :: Nil).length == nodes.length &&
                   parents(root).isEmpty &&
                   nodes.forall(n => parents(n).length == 1 || n == root)

      if(! isTree) {
        false
      }
      else {

        def projective(node:T, left:T, right:T): Boolean =
          lteq(left, node) && gteq(right, node) &&
          children(node).forall( c =>
            projective(c,
              if(gt(c, node)) node else left,
              if(lt(c, node)) node else right
          ))

        projective(root, root, nodes.max)
      }
    }
  }

//  def isFullyConnectedProjectiveTree[T <: Ordering](graph: Map[(T, T), Boolean]) = {
//    isFullyConnectedNonProjectiveTree(graph) && isProjectiveTree(graph)
//  }
//
//  def isProjectiveTree[T <: Ordering](graph: Map[(T, T), Boolean]) = {
//    val indexedEdges = (graph filter (_._2)).toList map (_._1)
//    (for (e1 <- indexedEdges;  e2 <- indexedEdges) yield (e1.,e2)).exists { case((i,j),(k,l)) =>
//      crosses(i,j,k,l)
//    }
//  }
//
//  def crosses[T <: Ordering](ii: T, ij: T, ik: T, il: T): Boolean = {
//    val (i,j) = if (ii < ij) (ii, ij) else (ij, ii)
//    val (k,l) = if (ik < il) (ik, il) else (il, ik)
//    (i < k && k < j && j < l) || (k < i && j < k && l < j)
//  }


}
