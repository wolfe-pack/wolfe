package ml.wolfe.nlp.syntax

/**
 * Created by narad on 12/5/14.
 */
trait ConstituentNode {

  def label:String

  def start: Int

  def end: Int

  def width = end - start

  def isNonterminal: Boolean = this match {
    case x: NonterminalNode => true
    case _ => false
  }

  def isPreterminal: Boolean = this match {
    case x: PreterminalNode => true
    case _ => false
  }
}

case class NonterminalNode(start: Int, end: Int, label: String, head: Option[HeadInfo] = None) extends ConstituentNode {

  override def isNonterminal = true

  override def isPreterminal = false
}

case class PreterminalNode(start: Int, end: Int, label: String, word: String) extends ConstituentNode {

  override def isNonterminal = false

  override def isPreterminal = true
}

case class HeadInfo(headWord: String, headIdx: Int, tokenIdx: Int)