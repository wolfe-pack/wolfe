package ml.wolfe.util

import scala.io.Source
import ml.wolfe.Wolfe

/**
 * Code for NLP datasets.
 */
object NLP {

  import scala.language.implicitConversions
  import Wolfe._

  trait Label {
    def label: Symbol
    override def hashCode() = label.hashCode()
    override def equals(obj: scala.Any) =
      obj.getClass == getClass && obj.asInstanceOf[Label].label == label
  }

  case class Chunk(label: Symbol) extends Label
  case class Tag(label: Symbol) extends Label
  case class DocLabel(label: Symbol) extends Label

  case class Token(word: String, tag: Tag = default, chunk: Chunk = default) {
    override def toString = s"$word/${ tag.label }/${ chunk.label }"
  }

  case class Sentence(tokens: Seq[Token]) {
    override def toString = tokens.view.mkString(" ")
    def toSentenceString = tokens.view.map(_.word).mkString(" ")
  }

  case class Doc(source: String, tokens: Seq[Token], label: DocLabel)

  implicit def labelToSymbol(l: Label) = l.label
  implicit def symbolToTag(l: Symbol) = Tag(l)
  implicit def stringToTag(l: String) = Tag(Symbol(l))
  implicit def symbolToChunk(l: Symbol) = Chunk(l)
  implicit def stringToChunk(l: String) = Chunk(Symbol(l))
  implicit def symbolToDocLabel(l: Symbol) = DocLabel(l)
  //implicit def stringToSymbol(l: String) = Symbol(l)



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
    Seq("B-NP","B-PP","I-NP","B-VP","I-VP","B-SBAR","O","B-ADJP","B-ADVP","I-ADVP","I-ADJP","I-SBAR","I-PP","B-PRT",
      "B-LST","B-INTJ","I-INTJ","B-CONJP","I-CONJP","I-PRT","B-UCP","I-UCP").map(c => Chunk(Symbol(c)))

  implicit val ptbTags =
    Seq("#", "$", "''", "(", ")", ",", ".", ":", "CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS",
      "MD", "NN", "NNP", "NNPS", "NNS", "PDT", "POS", "PRP", "PRP$", "RB", "RBR", "RBS", "RP", "SYM",
      "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB", "``").map(c => Tag(Symbol(c)))
}
