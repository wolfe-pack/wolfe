package ml.wolfe.nlp

import breeze.linalg.{SparseVector, DenseVector}
import ml.wolfe.{SimpleIndex, Index}


/**
 * Offsets for a natural language token.
 * @param start index of the initial character in the token.
 * @param end index of the final character in the token
 */
case class CharOffsets(start: Int, end: Int)

/**
 * A natural language token.
 * @param word word at token.
 * @param offsets character offsets
 * @param posTag part-of-speech tag at token.
 * @param lemma lemma at token.
 */
case class Token(word: String, offsets: CharOffsets, posTag: String = null, lemma: String = null) {
  def toTaggedText = word + "/" + posTag
  def sentence(implicit graph: ObjectGraph) =
    graph.receiveOrdered[Token, Sentence, Sentence]('tokens, this)((_, s) => s)
  def next(implicit graph: ObjectGraph) =
    graph.receiveOrdered[Token, Sentence, Option[Token]]('tokens, this)((i, s) => s.tokens.lift(i + 1))
  def prev(implicit graph: ObjectGraph) =
    graph.receiveOrdered[Token, Sentence, Option[Token]]('tokens, this)((i, s) => s.tokens.lift(i - 1))

}

/**
 * A sentence consisting of tokens.
 * @param tokens the tokens of the sentence.
 * @param syntax syntactic annotation for the sentence.
 * @param ie information extraction style annotation for the sentence
 */
case class Sentence(tokens: Seq[Token], syntax: SyntaxAnnotation = SyntaxAnnotation.empty, ie: IEAnnotation = IEAnnotation.empty) {
  def toText = tokens map (_.word) mkString " "
  def toTaggedText = tokens map (_.toTaggedText) mkString " "
  def document(implicit g:ObjectGraph) =
    g.receiveOrdered[Sentence,Document,Document]('sentences,this)((_,d) => d)
  def linkTokens(implicit graph: ObjectGraph) =
    graph.link1toNOrdered[Sentence, Token, Seq[Token]]('tokens, this, tokens)
  def size = tokens.size
}

/**
 * A document consisting of sentences.
 * @param source the source text.
 * @param sentences list of sentences.
 * @param ir information retrieval annotations for document.
 */
case class Document(source: String,
                    sentences: Seq[Sentence],
                    filename:Option[String] = None,
                    ir:IRAnnotation = IRAnnotation.empty) {
  def toText = sentences map (_.toText) mkString "\n"
  def toTaggedText = sentences map (_.toTaggedText) mkString "\n"
  def tokens = sentences flatMap (_.tokens)
  def $sentences(implicit g:ObjectGraph) =
    g.link1toNOrdered[Document,Sentence,Seq[Sentence]]('sentences, this, sentences)

}

/**
 * Class to represent IR information for a document
 * @param docLabel an optional document label.
 * @param bowVector a vectorized bag of word representation, for example using tf-idf counts.
 */
case class IRAnnotation(docLabel:Option[String] = None,
                        bowVector:Option[SparseVector[Double]] = None)

object IRAnnotation {
  val empty = IRAnnotation()
}


object Data {
  def main(args: Array[String]) {
    val source = "My name is Wolfe."

    val result = SISTAProcessors.mkDocument(source)

    implicit val graph = new SimpleObjectGraph

    val s = result.sentences.head
    //s.linkTokens(graph) //build graph

    println(s.tokens.head.sentence == s)
    println(s.tokens.head.next.get == s.tokens(1))

    //    val result2 = SISTAProcessors.annotate(source)
    //
    //    println(result2.toTaggedText)
    //    println(result2)


  }
}