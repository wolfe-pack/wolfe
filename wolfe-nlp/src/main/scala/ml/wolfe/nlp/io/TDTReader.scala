package ml.wolfe.nlp.io
import ml.wolfe.nlp.{CharOffsets, Document, IRAnnotation, Sentence, Token}

/**
 * Created by narad on 9/10/14.
 */
class TDTReader(dir: String, iformat: String = "UTF-8") extends Iterable[(Seq[String], Document)] {
  val DOC_PATTERN = """<TDTID>(.*)</TDTID>""".r
  val PARAGRAPH_PATTERN = """<P>(.*)</P>""".r

  val events = io.Source.fromFile(dir + "eventnums.lis").getLines().toArray
  val labels = io.Source.fromFile(dir + "tdt-corpus.judgments").getLines().filter(!_.startsWith("#")).map { line =>
    val cells = line.trim.replaceAll(" +", " ").split(" ")
    cells(1) ->(cells(0), cells(3))
  }.toArray

  def iterator: Iterator[(Seq[String], Document)] = {
    try {
      new ChunkReader(dir + "tdt-corpus.sgml", delim="</DOC>").map { chunk =>
        val docID = (DOC_PATTERN findFirstMatchIn chunk).get.group(1)
        val paragraphs = (PARAGRAPH_PATTERN findAllIn chunk).matchData.map(_.group(1).trim)
        val sents = paragraphs.map{p =>
          Sentence(tokens=p.split(" ").zipWithIndex.map{case(w,i) => Token(w,CharOffsets(i,i))})
        }.toSeq
        val entities = labels.filter(_._1 == docID).map(_._2._1).toSeq
        (entities, Document(source=null, sentences = sents, ir = IRAnnotation(docLabel = Some(docID))))
      }.iterator
    }
    catch {
      case e: Exception => {
        System.err.println("Error in XML reading:\n" + e.getStackTrace.mkString("\n"))
        Iterator[(Seq[String], Document)]()
      }
    }
  }

}

object TDTReader {

  def main(args: Array[String]) = {
    for (d <- new TDTReader(args(0)) if !d._1.isEmpty) {
      println(d._1)
      println(d._2)
      println
    }
  }
}