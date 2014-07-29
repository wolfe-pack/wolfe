package ml.wolfe.nlp

import scala.collection.mutable


case class CharOffsets(start: Int, end: Int)

/**
 * A natural language token.
 * @param word word at token.
 * @param offsets character offsets
 * @param posTag part-of-speech tag at token.
 * @param attributes collection of generic attributes.
 */
case class Token(word: String, offsets: CharOffsets, posTag: String = null, attributes: Attributes = Attributes.empty) {
  def toTaggedText = word + "/" + posTag
  def $sentence(implicit graph: ObjectGraph) = graph.receive[Token, Sentence]('tokens, this)
}

/**
 * A sentence consisting of tokens.
 * @param tokens the tokens of the sentence.
 * @param attributes collection of generic attributes.
 */
case class Sentence(tokens: Seq[Token], attributes: Attributes = Attributes.empty) {
  def toText = tokens map (_.word) mkString " "
  def toTaggedText = tokens map (_.toTaggedText) mkString " "
  def $tokens(implicit graph: ObjectGraph) = graph.link1toN[Sentence, Token, Seq[Token]]('tokens, this, tokens)
}

/**
 * A document consisting of sentences.
 * @param source the source text.
 * @param sentences list of sentences.
 * @param attributes collection of generic attributes.
 */
case class Document(source: String, sentences: Seq[Sentence], attributes: Attributes = Attributes.empty) {
  def toText = sentences map (_.toText) mkString "\n"
  def toTaggedText = sentences map (_.toTaggedText) mkString "\n"
  def tokens = sentences flatMap (_.tokens)
}

trait ObjectGraph {
  def link1toN[Parent, Child, I <: Iterable[Child]](rel: Any, parent: Parent, children: I): I
  def receive[Child, Parent](rel: Any, child: Child): Parent
}

class SimpleObjectGraph extends ObjectGraph {
  private val map = new mutable.HashMap[(Any, Any), Any]()
  def link1toN[Parent, Child, I <: Iterable[Child]](rel: Any, parent: Parent, children: I) = {
    for (c <- children) map(c -> rel) = parent
    children
  }
  def receive[Child, Parent](rel: Any, child: Child) = map(child -> rel).asInstanceOf[Parent]
}

object Data {
  def main(args: Array[String]) {
    val source = "My name is Wolfe."

    val result = SISTAProcessors.mkDocument(source)

    implicit val graph = new SimpleObjectGraph
    val s = result.sentences.head
    println(s.$tokens.head.$sentence == s)

//    val result2 = SISTAProcessors.annotate(source)
//
//    println(result2.toTaggedText)
//    println(result2)


  }
}