package ml.wolfe.nlp.io

import ml.wolfe.nlp._

import scala.collection.mutable.ListBuffer

/**
 * Created by narad on 8/12/14.
 */
class CoNLLReader(filename: String, delim:String="\t") extends Iterable[Sentence] {

  def iterator: Iterator[Sentence] = {
    val reader = new ChunkReader(filename)
    reader.iterator.map { s =>
      val lines = s.split("\n")
      val numFields = lines.head.split(delim).size
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

  def fromCoNLL2009(lines: IndexedSeq[String]): Sentence = {
    // ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
    val cells = lines.map(_.split("\t"))
    val cols = cells.head.size
    val tokens = cells.map { c =>
      Token(c(1), CharOffsets(c(0).toInt, c(0).toInt), posTag = c(4), lemma = c(2))
    }
    val preds = cells.zipWithIndex.filter(_._1(12) == "Y").map { case (l, i) => Predicate(i, tokens(i), l(13)) }
    val argsets = (14 to 13 + preds.size).map { i =>
      cells.zipWithIndex.flatMap { case (row, ri) => row(i) match {
        case "_" => None
        case x => Some(SemanticRole(ri, x))
      }
      }
    }
    val frames = preds.zip(argsets).map { case (p, a) => SemanticFrame(p, a) }
    assert(preds.size == argsets.size)
    val dependencies = DependencyTree(tokens, cells.zipWithIndex.map { case (c, i) => (i + 1, c(8).toInt, c(10)) })
    Sentence(tokens, syntax = SyntaxAnnotation(tree = null, dependencies = dependencies), ie = IEAnnotation.empty.copy(semanticFrames = frames))
  }

  def fromCoNLLX(lines: IndexedSeq[String]): Sentence = {
    val cells = lines.map(_.split("\t"))
    val tokens = cells.map { c =>
      Token(c(1), CharOffsets(c(0).toInt, c(0).toInt), posTag = c(4), lemma = c(2))
    }
    val dependencies = DependencyTree(tokens, cells.zipWithIndex.map { case (c, i) => (i + 1, c(6).toInt, c(7)) })
    Sentence(tokens, syntax = SyntaxAnnotation(tree = null, dependencies = dependencies))
  }

  def fromCoNLL2003(lines: IndexedSeq[String]): Sentence = {
    val cells = lines.map(_.split(" "))
    def join(sofar:List[Token],c:Array[String]):List[Token] = sofar match {
      case Nil => Token(c(0), CharOffsets(0, c(0).length), posTag = c(1)) :: Nil
      case h :: t => Token(c(0), CharOffsets(h.offsets.end + 1, h.offsets.end + 1 + c(0).length), posTag = c(1)) :: h :: t
    }

    val tokens = cells.foldLeft(List.empty[Token])(join).reverse
    val ner = cells.map(_.apply(3))
    val mentions = collectMentions(ner)
    Sentence(tokens.toIndexedSeq, ie = IEAnnotation(entityMentions = mentions.toIndexedSeq))
  }

  def fromCoNLL2000(lines: IndexedSeq[String]): Sentence = {
    ???
  }



  def collectMentions(t: Seq[String]): Seq[EntityMention] = {
    t.zipWithIndex.foldLeft(ListBuffer[EntityMention]())((mentions: ListBuffer[EntityMention], tokenWithIndex) => {
      val (label, ix) = tokenWithIndex
      val Array(prefix, labelType) = if (label == "O") Array("O", "O") else label.split("-")

      prefix match {
        case "O" => mentions
        case "B" => mentions :+ EntityMention(labelType,ix, ix+1)
        case "I" =>
          if (mentions.isEmpty) mentions :+ EntityMention(labelType,ix, ix+1)
          else {
            val last = mentions.last
            if (last.end == ix - 1 && last.label == labelType)
              mentions.updated(mentions.length - 1, mentions.last.expandRight(1))
            else mentions :+ EntityMention(labelType,ix, ix+1)
          }
      }}).toSeq
  }



}

object CoNLLReader {

  def asDocs(fileName:String,delim:String = " ") = new CoNLLReader(fileName,delim).map({
    case sentence => Document(sentence.toText,IndexedSeq(sentence))
  })


  def main(args: Array[String]) {
    val data = new CoNLLReader("/Users/sriedel/corpora/conll03/eng.train"," ").take(100).toIndexedSeq
    val doc = Document(data(1).toText,IndexedSeq(data(1)))
    println(data(1))


  }
}
