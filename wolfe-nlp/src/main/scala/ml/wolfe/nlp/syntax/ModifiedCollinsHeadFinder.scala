package ml.wolfe.nlp.syntax

import scala.collection.mutable

/**
 * Created by narad on 06/08/15.
 */

class ModifiedCollinsHeadFinder extends RuleBasedHeadFinder {

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
  rules.put("NP", Array(Array("rightdis", "NN", "NNP", "NNPS", "NNS", "NML", "NX", "JJR"), Array("left", "NP", "PRP"), Array("rightdis", "$", "ADJP", "JJP", "PRN", "FW"), Array("right", "CD"), Array("rightdis", "JJ", "JJS", "RB", "QP", "DT", "WDT", "RBR", "ADVP")))
  rules.put("NML", Array(Array("rightdis", "NN", "NNP", "NNPS", "NNS", "NX", "NML", "JJR"), Array("left", "NP", "PRP"), Array("rightdis", "$", "ADJP", "JJP", "PRN"), Array("right", "CD"), Array("rightdis", "JJ", "JJS", "RB", "QP", "DT", "WDT", "RBR", "ADVP")))
  rules.put("POSSP", Array(Array("right", "POS")))
  rules.put("ROOT", Array(Array("left", "S", "SQ", "SINV", "SBAR", "FRAG")))
  rules.put("TYPO", Array(Array("left", "NN", "NP", "NML", "NNP", "NNPS", "TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP", "VP", "ADJP", "JJP", "FRAG")))
  rules.put("ADV", Array(Array("right", "RB", "RBR", "RBS", "FW", "ADVP", "TO", "CD", "JJR", "JJ", "IN", "NP", "NML", "JJS", "NN")))
  rules.put("EDITED", Array(Array("left")))
  rules.put("VB", Array(Array("left", "TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP", "VP", "AUX", "AUXG", "ADJP", "JJP", "NN", "NNS", "JJ", "NP", "NNP")))
  rules.put("META", Array(Array("left")))
  rules.put("XS", Array(Array("right", "IN")))
}
