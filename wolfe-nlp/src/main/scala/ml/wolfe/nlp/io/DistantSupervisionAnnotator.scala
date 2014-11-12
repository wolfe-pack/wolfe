package ml.wolfe.nlp.io
import ml.wolfe.nlp.{Document, EntityMention, RelationMention, Sentence, SISTAProcessors}

/**
 * Created by narad on 10/29/14.
 */
class DistantSupervisionAnnotator(docs: Iterable[Document], kbFile: String) extends FreebaseReader(kbFile, useExisting = true) with Iterable[Document] {

  def iterator = docs.map { d =>
    d.copy(sentences = d.sentences.map { s =>
      val words = s.tokens.map(_.word)
      val relations = entityPairs(s).flatMap { case ((e1,i), (e2,j)) =>
        val e1str = words.slice(e1.start, e1.end).mkString(" ")
        val e2str = words.slice(e2.start, e2.end).mkString(" ")
        getRelation(e1str, e2str) match {
          case Some(r) => Some(RelationMention(r, i, j))
          case _=> None
        }
      }
      s.copy(ie = s.ie.copy(relationMentions = relations))
    })
  }.iterator

  // Should change to Iterator
  def entityPairs(s: Sentence): IndexedSeq[((EntityMention, Int), (EntityMention, Int))] = {
    for (x <- s.ie.entityMentions.zipWithIndex; y <- s.ie.entityMentions.zipWithIndex) yield (x,y)
  }

}

object DistantSupervisionAnnotator {

  def main(args: Array[String]) = {
    test
  }

  def test = {
    val sents = Array("Barack Obama was inaugurated as the 51st President of the United States of America and then New York New York.")
    val docs = sents.map(SISTAProcessors.annotate(_, posTagger = true, parser = true, lemmatizer = true, ner = true))
    val annotator = new DistantSupervisionAnnotator(docs, "/Users/narad/Desktop/fb_test.gz")
    annotator.test
    for (a <- annotator) {
      println(a)
    }
  }
}
