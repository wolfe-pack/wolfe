package ml.wolfe.nlp.io

import ml.wolfe.nlp._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Created by narad on 8/12/14.
 */
class CoNLLReader(filename: String, delim: String = "\t") extends Iterable[Sentence] {

  def isNumber(str: String): Boolean = {
    str.matches("[0-9]+")
  }

  def iterator: Iterator[Sentence] = {
    val reader = new ChunkReader(filename)
    reader.iterator.map { s =>
      val lines = s.split("\n")
      val cols = lines.head.split(delim)
      val numFields = cols.size
      //      if (isNumber(cols(1)) && isNumber(cols(2))) {
      //        // Identified as a file from the CoNLL 2011 Coreference Task
      //        fromCoNLL2011(lines)
      //      }
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
    val preds = cells.zipWithIndex.filter(_._1(12) == "Y").map { case (l, i) => Predicate(i+1, tokens(i), l(13)) }
    val argsets = (14 to 13 + preds.size).map { i =>
      cells.zipWithIndex.flatMap { case (row, ri) => row(i) match {
        case "_" => None
        case x => Some(SemanticRole(ri+1, x))
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
    def join(sofar: List[Token], c: Array[String]): List[Token] = sofar match {
      case Nil => Token(c(0), CharOffsets(0, c(0).length), posTag = c(1)) :: Nil
      case h :: t => Token(c(0), CharOffsets(h.offsets.end + 1, h.offsets.end + 1 + c(0).length), posTag = c(1)) :: h :: t
    }

    val tokens = cells.foldLeft(List.empty[Token])(join).reverse
    val ner = cells.map(_.apply(3))
    val mentions = CoNLLReader.collectMentions(ner)
    Sentence(tokens.toIndexedSeq, ie = IEAnnotation(entityMentions = mentions.toIndexedSeq))
  }

  def fromCoNLL2000(lines: IndexedSeq[String]): Sentence = {
    ???
  }
}

object CoNLLReader {

  def asDocs(fileName: String, delim: String = " ") = new CoNLLReader(fileName, delim).map({
    case sentence => Document(sentence.toText, IndexedSeq(sentence))
  })

  def collectMentions(t: Seq[String]): Seq[EntityMention] = {
    t.zipWithIndex.foldLeft(ListBuffer[EntityMention]())((mentions: ListBuffer[EntityMention], tokenWithIndex) => {
      val (label, ix) = tokenWithIndex
      val Array(prefix, labelType) = if (label == "O") Array("O", "O") else label.split("-")

      prefix match {
        case "O" => mentions
        case "B" => mentions :+ EntityMention(labelType, ix, ix + 1)
        case "I" =>
          if (mentions.isEmpty) mentions :+ EntityMention(labelType, ix, ix + 1)
          else {
            val last = mentions.last
            if (last.end == ix && last.label == labelType)
              mentions.updated(mentions.length - 1, mentions.last.expandRight(1))
            else mentions :+ EntityMention(labelType, ix, ix + 1)
          }
      }
    }).toSeq
  }

  def main(args: Array[String]) {
    val data = new CoNLLReader("/Users/sriedel/corpora/conll03/eng.train", " ").take(100).toIndexedSeq
    val doc = Document(data(1).toText, IndexedSeq(data(1)))
    println(data(1))
  }
}

class CoNLL2011Reader(filename: String, delim: String = "\t") extends Iterable[Document] {

  def iterator: Iterator[Document] = {
    val files = new ChunkReader(filename).toArray.map(_.split("\n").filter(!_.startsWith("#")).mkString("\n")).groupBy { c =>
      val l = c.split("\n")//.filter(!_.startsWith("#"))
      if (l.isEmpty || l.size == 1) {
        null
      }
      else {
        val first = l.head
        first.substring(0, first.indexOf(" "))
      }
    }
    files.keys.filter(_ != null).map{f => mkCoNLL2011Document(files(f))}.iterator
  }

  def mkCoNLL2011Document(chunks: Array[String]): Document = {
    val sents = chunks.map { chunk =>
      //      println(chunk + "\n")
      val grid = chunk.split("\n").map(_.replaceAll(" +", "\t").split("\t"))
      //      println(grid(0).mkString(", "))
      //      println("lines = " + grid.size)
      //      println("cols = " + grid(0).size)
      //      println
      val words = (0 until grid.size).map(grid(_)(3))
      val pos = (0 until grid.size).map(grid(_)(4))
      val lemma = (0 until grid.size).map(grid(_)(6))
      val tokens = words.zip(pos).zipWithIndex.map { case (p, i) => Token(p._1, CharOffsets(i, i + 1), p._2, lemma = lemma(i))}
      val csyntax = (0 until grid.size).map(grid(_)(5))
      val ner = ((0 until grid.size).map(grid(_)(10))).map(s => if (!s.contains("*")) s.replaceFirst("\\)", "*)") else s)

      val csyntaxCleaned = csyntax.mkString(" ").replaceAll("\\*", " * ").replaceAll("\\(", " ( ").replaceAll("\\)", " ) ").replaceAll(" +", " ")
      var tc = -1
      val csyntaxStr = csyntaxCleaned.map(c => if (c == '*') {tc += 1; "(" + pos(tc) + " " + words(tc) + ")"} else c.toString).mkString("")
      val ctree = ConstituentTree.stringToTree(csyntaxStr)
      assert(ctree != null, "Null constituent tree")

      val nerCleaned = ner.mkString(" ").replaceAll("\\*", " * ").replaceAll("\\(", " ( ").replaceAll("\\)", " ) ").replaceAll(" +", " ")

      var nc = -1
      val nerStr = "(XX " + nerCleaned.map(c => if (c == '*') {nc += 1; "(XX " + words(nc) + ")"} else c.toString).mkString("") + ")"
      val nertree = ConstituentTree.stringToTree(nerStr)
      assert(nertree != null, "Null NER tree")
      val mentions = nertree.toSpans.view.filter(_.label != "XX").map(s => EntityMention(s.label, s.start, s.end))

      val sense = (0 until grid.size).map(grid(_)(7))
      Sentence(tokens, syntax = SyntaxAnnotation(tree = ctree, dependencies = null))
    }.toIndexedSeq



    val mentions = new ArrayBuffer[CorefMention]
    val corefs = chunks.zipWithIndex.map { case(chunk, sidx) =>
      val grid = chunk.split("\n").map(_.replaceAll(" +", "\t").split("\t"))
      val slen = grid.size
      val CSTART_PATTERN = """\(([0-9]+)""".r
      val CEND_PATTERN = """([0-9]+)\)""".r
      val buffer = new ArrayBuffer[(Int, Int)]
      (0 until slen).foreach { i =>
        val ccell = grid(i).last
        buffer ++= (CSTART_PATTERN findAllIn ccell).matchData.toArray.map(m => (i, m.group(1).toInt))
        val imentions = (CEND_PATTERN findAllIn ccell).matchData.toArray.map(m => (i, m.group(1).toInt)).foreach { e =>
          val sidx = buffer.indexWhere(_._2 == e._2)
          val s = buffer(sidx)
          buffer.remove(sidx)
          mentions += CorefMention(s._2, sidx, s._1, e._1+1)
        }
      }
    }.toIndexedSeq
    Document(source = chunks.mkString("\n"), sentences = sents, coref = CorefAnnotation(mentions = mentions.toSeq))
  }
}


/*      (5  59
      (8 44
       9 44)
      10 59)
      */

//      println("STARTS: " + starts.mkString(", "))
//      println("ENDS: " + ends.mkString(", "))
//      var mentions = new ArrayBuffer[CorefMention]
//      for (w <- 0 until slen; i <- 0 until slen if (w+i) < slen) {
//
//      }
//      val mentions = starts.map { s =>
//        val e = ends.find(e => s._2 == e._2).get
//        ends = ends.filter(_ != e)
//        CorefMention(s._2, sidx, s._2, e._2)
//      }.toSeq


object CoNLL2011Reader extends App {
  for (c <- new CoNLL2011Reader(args(0))) {
    val dummy = "blah"
    //println(c) + "\n"
  }
}




//      cells.zipWithIndex.filter(_._1(6) == "Y").map { case (l, i) => Predicate(i+1, tokens(i), l(13)) }
//      val argsets = (14 to 13 + preds.size).map { i =>
//        cells.zipWithIndex.flatMap { case (row, ri) => row(i) match {
//          case "_" => None
//          case x => Some(SemanticRole(ri+1, x))
//        }
//        }
//      }
//      val frames = preds.zip(argsets).map { case (p, a) => SemanticFrame(p, a) }
