package ml.wolfe.nlp.io
import ml.wolfe.nlp._
import scala.collection.mutable.ArrayBuffer

/**
 * Created by narad on 8/4/14.
 */
class TreebankReader(filename: String, options: TreebankReaderOptions = new DefaultTreebankReaderOptions) extends Iterable[ConstituentTree] {
  private val TOKEN_PATTERN = """\(([^ \(]+) ([^ \)]+)\).*""".r
  private val CONSTITUENT_PATTERN = """\(([^ ]+) .*""".r
  private val EMPTY_PATTERN = """\( .*""".r
  private val DOUBLE_PAREN_PATTERN = """\(\(+.*""".r

  def iterator: Iterator[ConstituentTree] = {
    val transformer = new TreeTransformer(options)
    val text = "(DOC %s)".format(scala.io.Source.fromFile(filename).getLines().filter(!_.startsWith("*")).mkString)
    val t = stringToTree(text, options)
    t.children.map(transformer.transformTree(_)).iterator
  }

  def stringToTree(str: String, options: TreebankReaderOptions): ConstituentTree = {
    str match {
      case DOUBLE_PAREN_PATTERN() => {
        val children = subexpressions(str).map(stringToTree(_, options))
        return ConstituentTreeFactory.buildTree(label=options.DEFAULT_LABEL, children=children)
      }
      case TOKEN_PATTERN(tag, word) => {
        ConstituentTreeFactory.buildTree(label=tag, word=word)
      }
      case CONSTITUENT_PATTERN(label) => {
        val children = subexpressions(str).map(stringToTree(_, options))
        return ConstituentTreeFactory.buildTree(label=label, children=children)
      }
      case EMPTY_PATTERN() => {
        val children = subexpressions(str).map(stringToTree(_, options))
        return ConstituentTreeFactory.buildTree(label=options.DEFAULT_LABEL, children=children)
      }
      case _ => {
        if (str != null) System.err.println("Not recognized: %s".format(str))
        return null.asInstanceOf[ConstituentTree]
      }
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
      if (t.children.size == 1) t = t.removeTop
    }
    if (options.FIX_ROOT) {
      t = new ConstituentTree(NonterminalNode("TOP"), t.children)
    }
    if (options.COARSEN_LABELS) {
      t = t.coarsenLabels
    }
    if (options.UNIFY_NONTERMS) {
      t = t.nodemap(n => {
        if (n.isNonterminal) new NonterminalNode("X")
        else n
      })
    }
    return t
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

  def buildTree(label: String = "SPAN", word: String = "", children: List[ConstituentTree] = List()): ConstituentTree = {
    if (word == "") {
      new ConstituentTree(new NonterminalNode(label), children)
    }
    else {
      new ConstituentTree(new PreterminalNode(label, word), children)
    }
  }

  def constructFromSpans(spans: Array[Span], slen: Int, words: Array[String] = Array(), tags: Array[String] = Array()): ConstituentTree = {
    return ConstituentTreeFactory.buildTree(label="TOP", children=findChildren(spans.sortBy(sp => (sp.width * 1000) + sp.height).reverse, 0, slen, words, tags))
  }

  def findChildren(spans: Array[Span], start: Int, end: Int, words: Array[String] = Array(), tags: Array[String] = Array()): List[ConstituentTree] = {
    val children = new ArrayBuffer[ConstituentTree]
    val max = findMaxSpan(spans, start, end)
    max match {
      case None => {
        if (words.isEmpty || tags.isEmpty) {
          List.fill(end-start)(ConstituentTreeFactory.buildTree(label="TAG", word="TMP", children=List()))
        }
        else {
          (start until end).map { i => ConstituentTreeFactory.buildTree(label=tags(i), word=words(i), children=List()) }.toList
        }
      }
      case Some(maxspan) => {
        if (maxspan.start > start) {
          val leftChildren = findChildren(spans, start, maxspan.start, words, tags)
          if (leftChildren.size > 0) children ++= leftChildren
        }
        val cchildren = findChildren(spans.filter(_ != maxspan), maxspan.start, maxspan.end, words, tags)
        children += ConstituentTreeFactory.buildTree(label=maxspan.label, children=cchildren)
        if (maxspan.end < end) {
          val rightChildren = findChildren(spans, maxspan.end, end, words, tags)
          if (rightChildren.size > 0) children ++= rightChildren
        }
        children.toList
      }
    }
  }

  def findMaxSpan(spans: Array[Span], start: Int, end: Int): Option[Span] = {
    for (span <- spans) {
      if (span.start >= start && span.end <= end) return Some(span)
    }
    return None
  }
}
