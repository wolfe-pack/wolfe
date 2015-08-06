package ml.wolfe.nlp.syntax

import ml.wolfe.nlp.io.{ConstituentTreeFactory, DefaultTreebankReaderOptions, TreebankReader}

import scala.collection.mutable.HashMap

/**
 * Created by narad on 3/18/15.
 */

abstract class HeadFinder {

  def annotate(tree: ConstituentTree): ConstituentTree
}

abstract class RuleBasedHeadFinder extends HeadFinder {
  /*
  Search style declared similar to the Stanford head finding
  left: search left-to-right by category and then by position
  right: search right-to-left by category and then by position
  leftdis: search left-to-right by position and then by category
  rightdis: search right-to-left by category and then by position
  leftexcept: take the first thing from the left that isn't in the list
  rightexcept: take the first thing from the right that isn't in the list
   */
  val rules: HashMap[String, Array[Array[String]]]

  def annotate(tree: ConstituentTree): ConstituentTree = {
    tree.node match {
      case nonterminal: NonterminalNode => {
        val hidx = findHead(tree)
        val headedChildren = tree.children.map(annotate(_))
        val (headWord, tidx) = headedChildren(hidx).node match {
          case nt: NonterminalNode => (nt.headInfo.get.headWord, nt.headInfo.get.tokenIdx)
          case pt: PreterminalNode => (pt.word, pt.start)
        }
        tree.copy(node = nonterminal.copy(headInfo = Some(HeadInfo(headWord = headWord, headIdx = hidx, tokenIdx = tidx))), children = headedChildren)
      }
      case leaf: PreterminalNode => tree
      case _ => { System.err.println("Error - did not match tree node: " + tree.node); assert(false, ""); null.asInstanceOf[ConstituentTree] }
    }
  }

  private def findHead(tree: ConstituentTree): Int = {
    if (tree.children.size == 1) return 0
    if (rules.contains(tree.label)) {
      rules(tree.label).foreach{ search =>
        val guess = search.head match {
          case "left" => searchLeft(tree, search)
          case "right" => searchRight(tree, search)
          case "leftdis" => searchLeftDis(tree, search)
          case "rightdis" => searchRightDis(tree, search)
        }
        if (guess > -1) return guess
      }
    }
    // Best guess at a default
    if (rules.contains(tree.label)) {
      if (rules(tree.label).head.head.startsWith("l")) 0 else tree.children.size-1
    }
    else {
      tree.children.size-1
    }
  }

  def searchLeft(tree: ConstituentTree, search: Array[String]): Int = {
    for(cat <- search) {
      tree.children.zipWithIndex.foreach { case (child, cidx) =>
       if (child.label == cat) return cidx
      }
    }
    -1
  }

  def searchLeftDis(tree: ConstituentTree, search: Array[String]): Int = {
    tree.children.zipWithIndex.foreach { case (child, cidx) =>
      if (search.contains(child.label)) return cidx
    }
    -1
  }

  def searchRight(tree: ConstituentTree, search: Array[String]): Int = {
    for(cat <- search) {
      tree.children.zipWithIndex.reverse.foreach { case (child, cidx) =>
        if (child.label == cat) return cidx
      }
    }
    -1
  }

  def searchRightDis(tree: ConstituentTree, search: Array[String]): Int = {
    tree.children.zipWithIndex.reverse.foreach { case (child, cidx) =>
      if (search.contains(child.label)) return cidx
    }
    -1
  }
}










//  def main(args: Array[String]) = {
//    //val str = "(S (NP (DT the) (JJ quick) (JJ (AA (BB (CC brown)))) (NN fox)) (VP (VBD jumped) (PP (IN over) (NP (DT the) (JJ lazy) (NN dog)))) (. .))"
//    val str = ("(NP (NP (DT the) (NN city) (POS 's)) (NN blend))")
//    val tree = ConstituentTreeFactory.stringToTree(str).get
//    val ctree = annotate(tree)
//    println("Constituent Tree:\n" + ctree.toHeadedTreebankString)
//    val dtree = ctree.toDependencyTree
//    println("Dependency Tree:\n" + dtree)
//    println
//    println(dtree.shortestPath(0, 3))
//    println("-----------------------")
//    println(dtree.shortestPath(3, 0))
//    println("-----------------------")
//    println(dtree.shortestPath(3, 8))
//    println("-----------------------")
//    println(dtree.pathToString(dtree.shortestPath(3, 8).get))
//  }
