package ml.wolfe.nlp.io
import ml.wolfe.nlp._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import java.io.File

/**
 * Created by narad on 8/20/14.
 */
class OntoReader(dir: String, pattern: String = ".*") extends Iterable[Document] {
  // Regex for OntoNotes 5.0
  val ONTO_5_SINGLE = new Regex("""<COREF_ID=\"(.*?)\"_TYPE=\"(.*?)\">([^<>]+)</COREF>""")
  val ONTO_5_START  = new Regex("""<COREF_ID=\"(.*?)\"_TYPE=\"(.*?)\">([^<]+)""")
  val ONTO_5_END    = new Regex("""([^<>]+)</COREF>""")
  // Regex for Ontonotes 3.0 and earlier
  val ONTO_3_SINGLE = new Regex("""<ENAMEX_TYPE=\"(.*?)\">([^<>]+)</ENAMEX>""")
  val ONTO_3_START  = new Regex("""<ENAMEX_TYPE=\"(.*?)\">([^<]+)""")
  val ONTO_3_END    = new Regex("""([^<>]+)</ENAMEX>""")

  def iterator: Iterator[Document] = {
    println("Generating iterator in OntoReader...")
    val dreader = new DirectoryReader
    val fgroups = dreader.fileGroups(dir)
    fgroups.keys.filter(k => k.matches(pattern) && fgroups(k).size > 1).iterator.map { f =>
      if (new File(f + ".coref").exists()) {
        // OntoNotes 5.0
        mkOnto5Document(f + ".parse", f + ".coref", f + ".prop", f + ".sense")
      }
      else {
        // OntoNotes 2.0/3.0
        mkOnto2Document(f + ".parse", f + ".name")
      }
    }
  }

  def mkOnto5Document(parseFile: String, corefFile: String, propFile: String, senseFile: String): Document = {
    println("Making Onto Document...")
    val trees = new TreebankReader(parseFile).toIndexedSeq
    val corefs = mkIE(corefFile, propFile)
//    val frames = mkFrames(propFile)
    val sentences = trees.zip(corefs).map{ case(t, ie) =>
      Sentence(t.tokens.toIndexedSeq, syntax = SyntaxAnnotation(tree = t, dependencies = null), ie = ie)}
    Document(source = "", sentences = sentences)
  }

  def mkOnto2Document(parseFile: String, nerFile: String): Document = ???

  // Returns a list of sentence ID and frame pairs
  // Does not handle all the crazy forms:
  // bn/cnn/00/cnn_0005@0005@cnn@bn@en@on ... 13:0-rel 0:1*12:0-ARG0 14:1-ARG1 16:1-ARG2
  def mkFrames(propFile: String): Seq[(Int, SemanticFrame)] = {
    val F_PATTERN = """([0-9]+):([0-9]+)[^\-]*\-(.*)""".r
    val frames = scala.io.Source.fromFile(propFile).getLines.toSeq.map { line =>
      println(line)
      val cols = line.split(" ")
      val sid = cols(1).toInt
      val tid = cols(2).toInt
      val prop = cols(4)
      val sense = cols(5)
      val pred = Predicate(tid, Token("TMP", CharOffsets(-1,-1)), sense)
      val roles = (7 until cols.size).map { cidx =>
        val F_PATTERN(s,e,r) = cols(cidx)
        SemanticRole(start = s.toInt, role = r)
      }
      (sid, SemanticFrame(pred, roles))
    }
    frames
  }

  def mkIE(corefFile: String, propFile: String): IndexedSeq[IEAnnotation] = {
    val frames = mkFrames(propFile).groupBy(_._1)
    mkCoref(corefFile).zipWithIndex.map { case(ie, idx) =>
      ie.copy(semanticFrames = if (frames.contains(idx)) frames(idx).map(_._2).toIndexedSeq else IndexedSeq())
    }
  }

  def mkCoref(corefFile: String): IndexedSeq[IEAnnotation] = {
    scala.io.Source.fromFile(corefFile).getLines.toIndexedSeq.filter(!isMetaInfo(_)).map { line =>
      val entities = new ArrayBuffer[EntityMention]
      var count = 0
      var label = ""
      val words = removeDummyWords(line).replaceAll("COREF ID", "COREF_ID").replaceAll("NAMEX TYPE", "NAMEX_TYPE").replaceAll(" TYPE=", "_TYPE=").split(" ")
      var tokens = new ArrayBuffer[String]
      words.zipWithIndex.foreach { case(token, index) =>
        token match {
          case ONTO_5_SINGLE(corefID, entityLabel, entityString) => {
            entities += EntityMention(entityLabel, index, index+1)
          }
          case ONTO_5_START(corefID, entityLabel, entityString) => {
            tokens += entityString
            label = entityLabel
          }
          case ONTO_5_END(entityString) => {
            tokens += entityString
            entities += EntityMention(label, index+1-tokens.size, index+1)
            tokens.clear()
          }
          case ONTO_3_SINGLE(entityLabel, entityString) => {
            entities += EntityMention(entityLabel, index, index+1)
          }
          case ONTO_3_START(entityLabel, entityString) => {
            tokens += entityString
            label = entityLabel
          }
          case ONTO_3_END(entityString) => {
            tokens += entityString
            entities += EntityMention(label, index+1-tokens.size, index+1)
            tokens.clear()
          }
          case default => {
            if (tokens.size > 0) tokens += token
          }
        }
        count += 1
      }
      IEAnnotation(entityMentions = entities, relationMentions = null, eventMentions = null, semanticFrames = null)
    }
  }

  def isMetaInfo(str: String): Boolean = {
    str.startsWith("<TEXT") || str.startsWith("<DOC") || str.startsWith("</")
  }

  def removeDummyWords(str: String): String = {
    var s = str
    s = s.replaceAll("-LRB-", "")
    s = s.replaceAll("-LCB-", "")
    s = s.replaceAll("-RRB-", "")
    s = s.replaceAll("-RCB-", "")
    s = s.replaceAll(" +", " ")
    s.trim
  }
}


object OntoReader {

  def main(args: Array[String]) {
    for (o <- new OntoReader(args(0)); s <- o.sentences) {
      println("Tokens:\n" + s.tokens.mkString(" "))
      println("Parse:\n" + s.syntax.tree)
      println("Entities:\n" + s.ie.entityMentions.mkString(" "))
      println
    }
  }
}
