package ml.wolfe.nlp


import scala.language.implicitConversions
import scala.collection.mutable


/**
 * Offsets for a natural language token.
 * @param start index of the initial character in the token.
 * @param end index of the final character in the token
 */
case class CharOffsets(start: Int, end: Int)

case class SentenceTokenRelation() extends ObjectGraphRelation {
  type Parent = Sentence
  type Child = Token
}

case class DocumentSentenceRelation() extends ObjectGraphRelation {
  type Parent = Document
  type Child = Sentence
}

/**
 * A natural language token.
 * @param word word at token.
 * @param offsets character offsets
 * @param posTag part-of-speech tag at token.
 * @param lemma lemma at token.
 */
case class Token(word: String, offsets: CharOffsets, posTag: String = null, lemma: String = null) {
  def toTaggedText = word + "/" + posTag
  def sentence(implicit graph: ObjectGraph[SentenceTokenRelation]) =
    graph.receive(this)
  def next(implicit graph: ObjectGraph[SentenceTokenRelation]) =
    graph.receive(this).tokens.lift(idx + 1)
  def prev(implicit graph: ObjectGraph[SentenceTokenRelation]) =
    graph.receive(this).tokens.lift(idx - 1)
  def toPrettyString = if (posTag != null) word + "/" + posTag else word
  def idx = offsets.start // Should replace with index lookup in ObjectGraph

}

abstract class DataStructure {
  def toPrettyString : String
}

case class BasicToken(word: String) extends DataStructure {
  def toPrettyString = word
}

trait DataIterator {
  this: DataStructure =>
  protected type X = this.type
  case class ForwardIteratorRelation() extends ObjectGraphRelation {
    type Parent = X
    type Child = X
  }
  def hasNext(implicit graph: ObjectGraph[DataIterator#ForwardIteratorRelation]): Boolean = {
    graph.hasKey(this)

  }
  def next(implicit graph: ObjectGraph[DataIterator#ForwardIteratorRelation]): DataIterator#ForwardIteratorRelation#Parent = graph.receive(this)
}







object DataOld {
  def main(args: Array[String]): Unit = {

    case class FancierToken(word: String) extends DataStructure with DataIterator {
      def toPrettyString = word
    }

    case class FanciestToken(word: String)  extends DataStructure with DataIterator {
      def toPrettyString = word + "No!"
    }

    implicit val graph = new SimpleObjectGraph[DataIterator#ForwardIteratorRelation]


    val tokens = for (i <- 1 to 10) yield new FancierToken(i.toString)
    val tokens2 = for (i <- 1 to 10) yield new FanciestToken(i.toString)
    graph.link1to1(tokens(1),tokens(0))
    println(tokens(0).hasNext)
    println(tokens(0).next)
    println(tokens(0).next.getClass)
    graph.link1to1(tokens2(0),tokens(2))
    println(tokens(2).hasNext)
    println(tokens(2).next)
    //val test : FancierToken = tokens(0).next
    println(tokens(2).next.getClass)

  }
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
  def document(implicit graph: ObjectGraph[DocumentSentenceRelation]) =
    graph.receive(this)
  def linkTokens(implicit graph: ObjectGraph[SentenceTokenRelation]) =
    graph.link1toN(this, tokens)
  def size = tokens.size
  def offsets = CharOffsets(tokens.head.offsets.start,tokens.last.offsets.end)
  def toPrettyString = tokens.map(_.toPrettyString).mkString(" ")

  def toCoNLLString = {
    // ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
    val numPreds = ie.semanticFrames.size
    tokens.zipWithIndex.map { case(t,i) =>
      if (syntax.dependencies != null) {
        val head = syntax.dependencies.headOf(i+1).getOrElse(-1)
        val headLabel = syntax.dependencies.labelOf(i+1).getOrElse("null")
        val morph = "-|-|-|-"
        val sense = ie.semanticFrames.find(_.predicate.idx == i+1) match {
          case Some(frame) => frame.predicate.sense
          case _ => "_"
        }
        val hasPred = if (sense != "_") "Y" else "_"
        val roles = ie.semanticFrames.map(f => if (f.roles.exists(_.idx == i+1)) f.roles.find(_.idx == i+1).get.role else "_")
        Seq(i+1, t.word, t.lemma, t.lemma, t.posTag, t.posTag, morph, morph, head, head,
            headLabel, headLabel, hasPred, sense, roles.mkString("\t")).mkString("\t")
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
                    id: Option[String] = None,
                    ir: IRAnnotation = IRAnnotation.empty,
                    coref: CorefAnnotation = CorefAnnotation.empty,
                    discourse: DiscourseAnnotation = DiscourseAnnotation.empty) {

  def toText = sentences map (_.toText) mkString "\n"
  def toTaggedText = sentences map (_.toTaggedText) mkString "\n"
  def tokens = sentences flatMap (_.tokens)
  def linkSentences(implicit graph: ObjectGraph[DocumentSentenceRelation]) =
    graph.link1toN(this, sentences)
  def toPrettyString = sentences.map(_.toPrettyString).mkString("\n")

  def entityMentionsAsBIOSeq = sentences flatMap (_.entityMentionsAsBIOSeq)
  def tokenWords = sentences flatMap (s => s.tokens.map(_.word))

}

object Document {

  def apply(sentences:Seq[IndexedSeq[String]]) : Document = {
    val source = sentences.map(_.mkString(" ")).mkString(" ")
    var start = 0
    val resultSentences = for (s <- sentences) yield {
      val tokens = for (t <- s) yield {
        val tmp = Token(t, CharOffsets(start, start + t.length))
        start += t.size + 1
        tmp

      }
      Sentence(tokens)
    }
    Document(source, resultSentences.toIndexedSeq)
  }

  def apply(source: String) : Document = Document(source, IndexedSeq(Sentence(IndexedSeq(Token(source,CharOffsets(0,source.length))))))

  implicit def toDoc(source:String): Document = Document(source)

  /**
   * Creates a new Document based on the old document, where every token is surrounded by white spaces.
   * @param doc old Document
   * @return A normalised copy of the old Document
   */
  def normalizeDoc(doc:Document) = {
    Document(doc.sentences.map(_.tokens.map(_.word)))
  }

}




import scala.collection.mutable.HashMap








//object Data {
//  def main(args: Array[String]) {
//    val source = "Sally met Harry. She was a girl. He was not."
//
//    val result = SISTAProcessors.annotate(source, true, true, true, true, true, true)
//
//    println("coreference result: " + result.coref)
//
//    implicit val graph = new SimpleObjectGraph
//
//    val s = result.sentences.head
//
//    val cr = new ConllDocReader(null)
    //s.linkTokens(graph) //build graph

    //println(s.tokens.head.sentence == s)
    //println(s.tokens.head.next.get == s.tokens(1))

    //    val result2 = SISTAProcessors.annotate(source)
    //
    //    println(result2.toTaggedText)
    //    println(result2)

//
//  }
//}
