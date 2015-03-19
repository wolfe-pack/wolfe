package ml.wolfe.nlp.io

import ml.wolfe.nlp.converters.SISTAProcessors
import ml.wolfe.nlp.{Document, EntityMention, RelationMention, Sentence}
import java.io.{File, FileWriter}

/**
 * Created by narad on 10/29/14.
 */
class DistantSupervisionAnnotator { //extends FreebaseReader(filename = null, useExisting = true) {
  val fbr = FreebaseReader.loadFromDB()

  def annotate(docs: Iterable[Document]): Iterable[Document] = docs.map(annotate(_))

  def annotate(doc: Document): Document = {
    doc.copy(sentences = doc.sentences.map { s =>
      val words = s.tokens.map(_.word)
      val relations = entityPairs(s).flatMap { case ((e1,i), (e2,j)) =>
        val e1str = words.slice(e1.start, e1.end).mkString(" ")
        val e2str = words.slice(e2.start, e2.end).mkString(" ")
        fbr.getRelationFromNames(e1str, e2str) match {
          case Some(r) => Some(RelationMention(r, i, j))
          case _=> None
        }
      }
      s.copy(ie = s.ie.copy(relationMentions = relations))
    })
  }

  // Should change to Iterator
  def entityPairs(s: Sentence): IndexedSeq[((EntityMention, Int), (EntityMention, Int))] = {
    for (x <- s.ie.entityMentions.zipWithIndex; y <- s.ie.entityMentions.zipWithIndex) yield (x,y)
  }

}

object DistantSupervisionAnnotator extends App{

  val annotator = new DistantSupervisionAnnotator

  val input = new File(args(0))
  if (input.isFile) {
    annotateToFile(args(0))
  }
  else if (input.isDirectory) {
    input.listFiles.foreach(f => annotateToFile(f.toString()))
  }

  def annotateToFile(filename: String) = {
    val ann = annotateFile(filename)
    val out = new FileWriter(filename + ".ann")
    out.write(ann.toString)
    out.close()
  }

  def annotateFile(filename: String): Document = {
    val text = io.Source.fromFile(filename).getLines().mkString("\n")
    val doc = SISTAProcessors.annotate(text, posTagger = true, parser = true, lemmatizer = true, ner = true)
    annotator.annotate(doc)
  }

  def test = {
    val sents = Array("Barack Obama was inaugurated as the 51st President of the United States of America and then New York New York.")
    val docs = sents.map(SISTAProcessors.annotate(_, posTagger = true, parser = true, lemmatizer = true, ner = true))
//    annotator.test
//    for (a <- annotator.annotate(docs)) {
//      println(a)
//    }
  }
}
