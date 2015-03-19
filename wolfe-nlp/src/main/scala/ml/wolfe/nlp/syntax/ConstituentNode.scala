package ml.wolfe.nlp.syntax

/**
 * Created by narad on 12/5/14.
 */
trait ConstituentNode {

  def label:String

  def isNonterminal: Boolean = this match {
    case x: NonterminalNode => true
    case _ => false
  }

  def isPreterminal: Boolean = this match {
    case x: PreterminalNode => true
    case _ => false
  }
}

case class NonterminalNode(label: String, head: Int = -1, headWord: String = "") extends ConstituentNode {

  override def isNonterminal = true

  override def isPreterminal = false
}

case class PreterminalNode(label: String, word: String) extends ConstituentNode {

  override def isNonterminal = false

  override def isPreterminal = true
}
