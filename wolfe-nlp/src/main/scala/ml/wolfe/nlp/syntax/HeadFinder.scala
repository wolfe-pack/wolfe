package ml.wolfe.nlp.syntax

import ml.wolfe.nlp.io.{DefaultTreebankReaderOptions, TreebankReader}

import scala.collection.mutable

/**
 * Created by narad on 3/18/15.
 */


object ModifiedCollinsHeadFinder {

  /*
  Search style declared similar to the Stanford head finding
  left: search left-to-right by category and then by position
  right: search right-to-left by category and then by position
  leftdis: search left-to-right by position and then by category
  rightdis: search right-to-left by category and then by position
  leftexcept: take the first thing from the left that isn't in the list
  rightexcept: take the first thing from the right that isn't in the list
   */

  val rules = new mutable.HashMap[String, Array[Array[String]]]
  rules.put("ADJP", Array(Array("left", "$"), Array("rightdis", "NNS", "NN", "JJ", "QP", "VBN", "VBG"), Array("left", "ADJP"), Array("rightdis", "JJP", "JJR", "JJS", "DT", "RB", "RBR", "CD", "IN", "VBD"), Array("left", "ADVP", "NP")))
  rules.put("JJP", Array(Array("left", "NNS", "NN", "$", "QP", "JJ", "VBN", "VBG", "ADJP", "JJP", "JJR", "NP", "JJS", "DT", "FW", "RBR", "RBS", "SBAR", "RB")))  // JJP is introduced for NML-like adjective phrases in Vadas' treebank Chris wishes he hadn't used JJP which should be a POS-tag.
  rules.put("ADVP", Array(Array("left", "ADVP", "IN"), Array("rightdis", "RB", "RBR", "RBS", "JJ", "JJR", "JJS"), Array("rightdis", "RP", "DT", "NN", "CD", "NP", "VBN", "NNP", "CC", "FW", "NNS", "ADJP", "NML")))
  rules.put("CONJP", Array(Array("right", "CC", "RB", "IN")))
  rules.put("FRAG", Array(Array("right"))) // crap
  rules.put("INTJ", Array(Array("left")))
  rules.put("LST", Array(Array("right", "LS", ":")))
  rules.put("NAC", Array(Array("left", "NN", "NNS", "NML", "NNP", "NNPS", "NP", "NAC", "EX", "$", "CD", "QP", "PRP", "VBG", "JJ", "JJS", "JJR", "ADJP", "JJP", "FW")))
  rules.put("NX", Array(Array("right", "NP", "NX")))
  rules.put("PP", Array(Array("right", "IN", "TO", "VBG", "VBN", "RP", "FW", "JJ", "SYM"), Array("left", "PP")))
  rules.put("PRN", Array(Array("left", "VP", "NP", "PP", "SQ", "S", "SINV", "SBAR", "ADJP", "JJP", "ADVP", "INTJ", "WHNP", "NAC", "VBP", "JJ", "NN", "NNP")))
  rules.put("PRT", Array(Array("right", "RP")))
  rules.put("QP", Array(Array("left", "$", "IN", "NNS", "NN", "JJ", "CD", "PDT", "DT", "RB", "NCD", "QP", "JJR", "JJS")))
  rules.put("RRC", Array(Array("left", "RRC"), Array("right", "VP", "ADJP", "JJP", "NP", "PP", "ADVP")))
  rules.put("S", Array(Array("left", "TO", "VP", "S", "FRAG", "SBAR", "ADJP", "JJP", "UCP", "NP")))
  rules.put("SBAR", Array(Array("left", "WHNP", "WHPP", "WHADVP", "WHADJP", "IN", "DT", "S", "SQ", "SINV", "SBAR", "FRAG")))
  rules.put("SBARQ", Array(Array("left", "SQ", "S", "SINV", "SBARQ", "FRAG", "SBAR")))
  rules.put("SINV", Array(Array("left", "VBZ", "VBD", "VBP", "VB", "MD", "VBN", "VP", "S", "SINV", "ADJP", "JJP", "NP")))
  rules.put("SQ", Array(Array("left", "VBZ", "VBD", "VBP", "VB", "MD", "AUX", "AUXG", "VP", "SQ")))  // TODO: Should maybe put S before SQ for tag questions. Check.
  rules.put("UCP", Array(Array("right")))
  rules.put("VP", Array(Array("left", "TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP", "VP", "AUX", "AUXG", "ADJP", "JJP", "NN", "NNS", "JJ", "NP", "NNP")))
  rules.put("WHADJP", Array(Array("left", "WRB", "WHADVP", "RB", "JJ", "ADJP", "JJP", "JJR")))
  rules.put("WHADVP", Array(Array("right", "WRB", "WHADVP")))
  rules.put("WHNP", Array(Array("left", "WDT", "WP", "WP$", "WHADJP", "WHPP", "WHNP")))
  rules.put("WHPP", Array(Array("right", "IN", "TO", "FW")))
  rules.put("X", Array(Array("right", "S", "VP", "ADJP", "JJP", "NP", "SBAR", "PP", "X")))
  rules.put("NP", Array(Array("rightdis", "NN", "NNP", "NNPS", "NNS", "NML", "NX", "POS", "JJR"), Array("left", "NP", "PRP"), Array("rightdis", "$", "ADJP", "JJP", "PRN", "FW"), Array("right", "CD"), Array("rightdis", "JJ", "JJS", "RB", "QP", "DT", "WDT", "RBR", "ADVP")))
  rules.put("NML", Array(Array("rightdis", "NN", "NNP", "NNPS", "NNS", "NX", "NML", "POS", "JJR"), Array("left", "NP", "PRP"), Array("rightdis", "$", "ADJP", "JJP", "PRN"), Array("right", "CD"), Array("rightdis", "JJ", "JJS", "RB", "QP", "DT", "WDT", "RBR", "ADVP")))
  rules.put("POSSP", Array(Array("right", "POS")))
  rules.put("ROOT", Array(Array("left", "S", "SQ", "SINV", "SBAR", "FRAG")))
  rules.put("TYPO", Array(Array("left", "NN", "NP", "NML", "NNP", "NNPS", "TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP", "VP", "ADJP", "JJP", "FRAG")))
  rules.put("ADV", Array(Array("right", "RB", "RBR", "RBS", "FW", "ADVP", "TO", "CD", "JJR", "JJ", "IN", "NP", "NML", "JJS", "NN")))
  rules.put("EDITED", Array(Array("left")))
  rules.put("VB", Array(Array("left", "TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP", "VP", "AUX", "AUXG", "ADJP", "JJP", "NN", "NNS", "JJ", "NP", "NNP")))
  rules.put("META", Array(Array("left")))
  rules.put("XS", Array(Array("right", "IN")))

  def annotate(tree: ConstituentTree): ConstituentTree = {
    tree.node match {
      case nonterminal: NonterminalNode => {
        val hidx = findHead(tree)
        val headedChildren = tree.children.map(annotate(_))
        val headWord = headedChildren(hidx).node match {
          case nt: NonterminalNode => nt.headWord
          case pt: PreterminalNode => pt.word
        }
        tree.copy(node = nonterminal.copy(head = hidx, headWord = headWord), children = headedChildren)
      }
      case leaf: PreterminalNode => tree
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
//          case "leftexcept" => searchLeftExcept(tree, search)
//          case "rightexcept" => searchRightExcept(tree, search)
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


  def main(args: Array[String]) = {
    val str = "((S (NP (DT the) (JJ quick) (JJ (AA (BB (CC brown)))) (NN fox)) (VP (VBD jumped) (PP (IN over) (NP (DT the) (JJ lazy) (NN dog)))) (. .)))"
    val reader = new TreebankReader(filename = null)
    val tree = reader.stringToTree(str, new DefaultTreebankReaderOptions)
    val atree = annotate(tree)
    for (i <- 0 until atree.length; j <- 1 to atree.length) {
      if (atree.containsSpan(i,j)) println("(%d,%d,%s) = %s".format(i, j, atree.labelsOfSpan(i, j).take(1), atree.headOf(i, j)))
    }
  }

}
