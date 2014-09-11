package ml.wolfe.nlp.io

import ml.wolfe.nlp._

/**
 * Created by narad on 8/12/14.
 */
class CoNLLReader(filename: String) extends Iterable[Sentence]{

  def iterator: Iterator[Sentence] = {
    val reader = new ChunkReader(filename)
    reader.iterator.map { s =>
      val lines = s.split("\n")
      val numFields = lines.head.split("\t").size
      if (numFields >= 12)
      // Identified as a file from the CoNLL SRL tasks of 2008/2009
        fromCoNLL2009(lines)
      else if (numFields == 10)
      // Identified as a file from the CoNLL-X Dependency Parsing shared task (2006/2007)
        fromCoNLLX(lines)
      else if (numFields == 4)
      // Identified as a file from the CoNLL 2003 NER shared task
        fromCoNLL2003(lines)
      else
      // Identified as a file from the CoNLL 2000 part-of-speech tagging shared task
        fromCoNLL2000(lines)
    }
  }

  def fromCoNLL2009(lines: Seq[String]): Sentence = {
    // ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
    val cells = lines.map(_.split("\t"))
    val tokens = cells.map {c =>
      Token(c(1), CharOffsets(c(0).toInt,c(0).toInt), posTag = c(4), lemma = c(2))
    }
    val dependencies = DependencyTree(tokens, cells.zipWithIndex.map{ case(c,i) => (i+1, c(8).toInt, c(10)) })
    Sentence(tokens, syntax = SyntaxAnnotation(tree=null, dependencies=dependencies))
  }

  def fromCoNLLX(lines: Seq[String]): Sentence = {
    val cells = lines.map(_.split("\t"))
    val tokens = cells.map {c =>
      Token(c(1), CharOffsets(c(0).toInt,c(0).toInt), posTag = c(4), lemma = c(2))
    }
    val dependencies = DependencyTree(tokens, cells.zipWithIndex.map{ case(c,i) => (i+1, c(6).toInt, c(7)) })
    Sentence(tokens, syntax = SyntaxAnnotation(tree=null, dependencies=dependencies))
  }

  def fromCoNLL2003(lines: Seq[String]): Sentence = {
    ???
  }

  def fromCoNLL2000(lines: Seq[String]): Sentence = {
    ???
  }


}