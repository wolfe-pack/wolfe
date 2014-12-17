package ml.wolfe.nlp

import breeze.linalg.{SparseVector, DenseVector}
import ml.wolfe.{SimpleIndex, Index}

import scala.collection.mutable


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
  def toPrettyString = if (posTag != null) word + "/" + posTag else word
  def idx = offsets.start // Should replace with index lookup in ObjectGraph

}

/**
 * A sentence consisting of tokens.
 * @param tokens the tokens of the sentence.
 * @param syntax syntactic annotation for the sentence.
 * @param ie information extraction style annotation for the sentence
 */
case class Sentence(tokens: IndexedSeq[Token], syntax: SyntaxAnnotation = SyntaxAnnotation.empty, ie: IEAnnotation = IEAnnotation.empty) {
  def toText = tokens map (_.word) mkString " "
  def toTaggedText = tokens map (_.toTaggedText) mkString " "
  def document(implicit g:ObjectGraph) =
    g.receiveOrdered[Sentence,Document,Document]('sentences,this)((_,d) => d)
  def linkTokens(implicit graph: ObjectGraph) =
    graph.link1toNOrdered[Sentence, Token, IndexedSeq[Token]]('tokens, this, tokens)
  def size = tokens.size
  def offsets = CharOffsets(tokens.head.offsets.start,tokens.last.offsets.end)
  def toPrettyString = tokens.map(_.toPrettyString).mkString(" ")

  def toCoNLLString = {
    tokens.zipWithIndex.map { case(t,i) =>
      if (syntax.dependencies != null) {
        val head = syntax.dependencies.headOf(i+1).getOrElse(-1)
        "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%d".format(i+1, t.word, t.lemma, t.lemma, t.posTag, t.posTag, head, head)
      }
      else {
        "%d\t%s\t%s\t%s\t%s\t%s".format(i+1, t.word, t.lemma, t.lemma, t.posTag, t.posTag)
      }
    }.mkString("\n")
  }

  /**
   * Return a representation of the entity mentions as a sequence of BIO labels. This representation
   * is different from CoNLL in that every mention begins with B-X.
   */
  def entityMentionsAsBIOSeq = {
    val tokenIndex2Label = new mutable.HashMap[Int,String]() withDefaultValue "O"
    for (m <- ie.entityMentions) {
      tokenIndex2Label(m.start) = "B-" + m.label
      for (i <- m.start + 1 until m.end) tokenIndex2Label(i) = "I-" + m.label
    }
    for (i <- 0 until tokens.size) yield tokenIndex2Label(i)
  }

}

/**
 * A document consisting of sentences.
 * @param source the source text.
 * @param sentences list of sentences.
 * @param ir information retrieval annotations for document.
 */
case class Document(source: String,
                    sentences: IndexedSeq[Sentence],
                    filename: Option[String] = None,
                    ir: IRAnnotation = IRAnnotation.empty,
                    coref: CorefAnnotation = CorefAnnotation.empty) {

  def toText = sentences map (_.toText) mkString "\n"
  def toTaggedText = sentences map (_.toTaggedText) mkString "\n"
  def tokens = sentences flatMap (_.tokens)
  def $sentences(implicit g:ObjectGraph) =
    g.link1toNOrdered[Document,Sentence,Seq[Sentence]]('sentences, this, sentences)
  def toPrettyString = sentences.map(_.toPrettyString).mkString("\n")

  def entityMentionsAsBIOSeq = sentences flatMap (_.entityMentionsAsBIOSeq)
  def tokenWords = sentences flatMap (s => s.tokens.map(_.word))

}

/**
 * Class to represent coreference annotation
 * @param mentions sequence of CorefMention elements
 */
case class CorefAnnotation(mentions: Seq[CorefMention] = Seq.empty)

object CorefAnnotation {
  val empty = CorefAnnotation()

}

/**
 * Class to represent coreference mention
 * @param clusterID ID of the cluster (chain) of connected mentions
 * @param sentence sentence index
 * @param head head of the coreference mention
 * @param start starting index
 * @param end ending index (points to the first token AFTER the mention)
 */
case class CorefMention(clusterID: Int, sentence: Int, head: Int, start: Int, end: Int)

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
    val source = "Sally met Harry. She was a girl. He was not."

    val result = SISTAProcessors.annotate(source, true, true, true, true, true, true)

    println("coreference result: " + result.coref)

    implicit val graph = new SimpleObjectGraph

    val s = result.sentences.head
    //s.linkTokens(graph) //build graph

    //println(s.tokens.head.sentence == s)
    //println(s.tokens.head.next.get == s.tokens(1))

    //    val result2 = SISTAProcessors.annotate(source)
    //
    //    println(result2.toTaggedText)
    //    println(result2)


  }
}
