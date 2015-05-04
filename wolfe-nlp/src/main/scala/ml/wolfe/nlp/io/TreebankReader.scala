package ml.wolfe.nlp.io
import ml.wolfe.nlp._
import ml.wolfe.nlp.syntax.{ConstituentSpan, PreterminalNode, NonterminalNode, ConstituentTree}
import scala.collection.mutable.ArrayBuffer

/**
 * Created by narad on 8/4/14.
 */
class TreebankReader(filename: String, options: TreebankReaderOptions = new DefaultTreebankReaderOptions) extends Iterable[ConstituentTree] {

  def iterator: Iterator[ConstituentTree] = {
    val transformer = new TreeTransformer(options)
    val text = "(DOC %s)".format(scala.io.Source.fromFile(filename).getLines().filter(!_.startsWith("*")).mkString)
    val t = ConstituentTreeFactory.stringToTree(text).get
    t.children.map(transformer.transformTree(_)).iterator
  }
}


class TreeTransformer(options: TreebankReaderOptions) {

  def transformTree(tree: ConstituentTree): ConstituentTree = {
    var t = tree
    if (options.REMOVE_NONES)
      t = t.removeNones()
    if (options.REMOVE_UNARY_CHAINS)
      t = t.removeUnaryChains()
    if (options.BINARIZE)
      t = t.binarize(options.BINARIZE_MODE)
    if (options.REMOVE_TOP) {
      if (t.children.size == 1) t = t.removeTopNode
    }
    if (options.FIX_ROOT) {
      t = new ConstituentTree(NonterminalNode(start = 0, end = t.length, label = "TOP"), t.children)
    }
    if (options.COARSEN_LABELS) {
      t = t.coarsenLabels
    }
    if (options.UNIFY_NONTERMS) {
      t = t.nodemap(n => {
        if (n.isNonterminal) new NonterminalNode(start = n.start, end = n.end, label = "X")
        else n
      })
    }
    t
  }

}

abstract class TreebankReaderOptions {

  def BINARIZE: Boolean

  def BINARIZE_MODE: String

  def COARSEN_LABELS: Boolean

  def DEFAULT_LABEL: String

  def FIX_ROOT: Boolean

  def REMOVE_NONES: Boolean

  def REMOVE_TOP: Boolean

  def REMOVE_UNARY_CHAINS: Boolean

  def UNIFY_NONTERMS: Boolean
}

object TreebankReaderOptions {

  def default = new DefaultTreebankReaderOptions
}

class DefaultTreebankReaderOptions extends TreebankReaderOptions {

  lazy val BINARIZE: Boolean = false

  lazy val BINARIZE_MODE = "RIGHT_0MARKOV"

  lazy val COARSEN_LABELS: Boolean = true

  lazy val DEFAULT_LABEL: String = "TOP"

  lazy val FIX_ROOT: Boolean = false

  lazy val REMOVE_NONES: Boolean = true

  lazy val REMOVE_TOP: Boolean = false

  lazy val REMOVE_UNARY_CHAINS: Boolean = false

  lazy val UNIFY_NONTERMS: Boolean = false
}


object ConstituentTreeFactory {

  private val TOKEN_PATTERN = """\(([^ \(]+) ([^ \)]+)\).*""".r
  private val CONSTITUENT_PATTERN = """\(([^ ]+) .*""".r
  private val EMPTY_PATTERN = """\( .*""".r
  private val DOUBLE_PAREN_PATTERN = """\(\(+.*""".r



  def stringToTree(str: String, leftMost: Int = 0): Option[ConstituentTree] = {
    val cleaned = str.trim.replaceAll("\\( +", "(").replaceAll("\\) +\\)", "))")
    val t = cleaned match {
      case DOUBLE_PAREN_PATTERN() => {
        val children = findChildren(subexpressions(str), leftMost = leftMost)
        Some(ConstituentTreeFactory.buildTree(start = leftMost, end = children.last.end, children=children))
      }
      case TOKEN_PATTERN(tag, word) => {
 //       println("TOKEN")
        Some(ConstituentTreeFactory.buildTree(start = leftMost, end = leftMost + 1, label=tag, word = Some(word)))
      }
      case CONSTITUENT_PATTERN(label) => {
//        println("CONST with label " + label + ".")
        val children = findChildren(subexpressions(str), leftMost = leftMost)
 //       println("children: " + children.mkString("\n"))
        Some(ConstituentTreeFactory.buildTree(start = leftMost, end = children.last.end, label = label, children = children))
      }
      case _ => None
    }
//    println("RETURNING TREE: " + t)
    t
  }

  def findChildren(strs: List[String], leftMost: Int): List[ConstituentTree] = {
    var tmpLeftMost = leftMost
    strs.map { s =>
      val child = stringToTree(s, leftMost = tmpLeftMost).get
      tmpLeftMost = child.end
      child
    }
  }

  def subexpressions(str: String, ldelim: Char='(', rdelim: Char=')'): List[String] = {
    val subs = new ArrayBuffer[String]
    var count = -1; var start = 0
    str.zipWithIndex.foreach { case(letter, index) =>
      if (letter == ldelim) {
        count += 1
        if (count == 1)
          start = index
      }
      else if (letter == rdelim) {
        count -= 1
        if (count == 0)
          subs += str.substring(start, index+1)
      }
    }
    subs.toList
  }




  // Methods for building ConstituentTree from spans

  def constructFromSpans(spans: Array[ConstituentSpan], slen: Int, words: Array[String] = Array(), tags: Array[String] = Array()): ConstituentTree = {
    ConstituentTreeFactory.buildTree(0, slen+1, label="TOP", children=findChildren(spans.sortBy(sp => (sp.width * 1000) + sp.height).reverse, 0, slen, words, tags))
  }

  def buildTree(start: Int, end: Int, label: String = "SPAN", word: Option[String] = None, children: List[ConstituentTree] = List()): ConstituentTree = {
    word match {
      case Some(w) => new ConstituentTree(new PreterminalNode(start = start, end = end, label = label, word = w), children)
      case None => new ConstituentTree(new NonterminalNode(start = start, end = end, label = label), children)
    }
  }


  def findChildren(spans: Array[ConstituentSpan], start: Int, end: Int, words: Array[String] = Array(), tags: Array[String] = Array()): List[ConstituentTree] = {
    val children = new ArrayBuffer[ConstituentTree]
    val max = spans.find(s => s.start >= start && s.end <= end)
    max match {
      case None => {
          (start until end).map { i => ConstituentTreeFactory.buildTree(start = i, end = i+1, label=tags(i), word=Some(words(i)), children=List()) }.toList
      }
      case Some(maxspan) => {
        if (maxspan.start > start) {
          val leftChildren = findChildren(spans, start, maxspan.start, words, tags)
          if (leftChildren.size > 0) children ++= leftChildren
        }
        val cchildren = findChildren(spans.filter(_ != maxspan), maxspan.start, maxspan.end, words, tags)
        children += ConstituentTreeFactory.buildTree(start = maxspan.start, end = maxspan.end, label=maxspan.label, children=cchildren)
        if (maxspan.end < end) {
          val rightChildren = findChildren(spans, maxspan.end, end, words, tags)
          if (rightChildren.size > 0) children ++= rightChildren
        }
        children.toList
      }
    }
  }
}

