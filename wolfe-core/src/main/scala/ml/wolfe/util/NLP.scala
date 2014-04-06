package ml.wolfe.util

import scala.io.Source

/**
 * Code for NLP datasets.
 */
object NLP {

  import scala.language.implicitConversions

  trait Label {def label: String }

  case class Chunk(label: String) extends Label
  case class Tag(label: String) extends Label
  case class DocLabel(label: String) extends Label

  class Default
  object default extends Default
  implicit def toDefaultValue[T <: AnyRef](default:Default) = null

  case class Token(word: String, tag: Tag = default, chunk: Chunk = default) {
    override def toString = s"$word/${ tag.label }/${ chunk.label }"
  }
  case class Sentence(tokens: Seq[Token]) {
    override def toString = tokens.view.mkString(" ")
    def toSentenceString = tokens.view.map(_.word).mkString(" ")
  }
  case class Doc(source: String, tokens: Seq[Token], label: DocLabel)

  implicit def labelToString(l: Label) = l.label
  implicit def stringToTag(l: String) = Tag(l)
  implicit def stringToChunk(l: String) = Chunk(l)
  implicit def stringToDocLabel(l: String) = DocLabel(l)

  /**
   * Takes an iterator over lines and groups this according to a delimiter line.
   */
  def groupLines(lines: Iterator[String], delim: String = ""): Seq[Seq[String]] = {
    groupLinesList(lines, delim).reverse.map(_.reverse)
  }

  def groupLinesList(lines: Iterator[String], delim: String = ""): List[List[String]] = {
    lines.foldLeft(List(List.empty[String])) {
      (result, line) => if (line == delim) Nil :: result else (line :: result.head) :: result.tail
    }
  }

  def loadCoNLL[T](lines: Iterator[String])(mapper: Array[String] => T): List[List[T]] =
    groupLinesList(lines).reverse.map(_.reverse.map(_.split("\\s+")).map(mapper))

  def loadCoNLL[T](fileOrClassPath: String)(mapper: Array[String] => T): List[List[T]] =
    loadCoNLL(Source.fromInputStream(Util.getStreamFromClassPathOrFile(fileOrClassPath)).getLines())(mapper)

  implicit val conllChunks =
    Seq("O", "B-VP", "B-NP", "B-PP", "I-VP", "I-NP", "I-PP", "B-SBAR", "I-SBAR", "B-ADJP", "I-ADJP").map(Chunk)

  implicit val ptbTags =
    Seq("#", "$", "''", "(", ")", ",", ".", ":", "CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS",
      "MD", "NN", "NNP", "NNPS", "NNS", "PDT", "POS", "PRP", "PRP$", "RB", "RBR", "RBS", "RP", "SYM",
      "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB", "``").map(Tag)


}
