package ml.wolfe.nlp.io

import java.io.{FilenameFilter, File}

import com.fasterxml.jackson.databind.ObjectMapper
import ml.wolfe.nlp._

import scala.util.parsing.json.JSON

/**
 * @author rockt
 * @author mbosnjak
 */
object LoadBioNLP extends App {
  def apply(pathToBioNLP: String): Seq[Document] = {
    val jsonFiles = new File(pathToBioNLP).listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".json")
    }).toList

    jsonFiles map jsonFileToWolfeDoc
  }

  class Extractor[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }
  object MapExtractor extends Extractor[Map[String, Any]]
  object ListExtractor extends Extractor[List[Any]]

  def jsonFileToWolfeDoc(file: File): Document = {
    val mapper = new ObjectMapper()
    JSON.globalNumberParser = {x: String => Integer.parseInt(x)}
    val parsed = JSON.parseFull(scala.io.Source.fromFile(file).getLines().mkString("\n"))

    val MapExtractor(attrMap) = parsed.get
    val txt = attrMap("txt").toString
    val ListExtractor(sentences) = attrMap("sentences")

    val wolfeSentences =
      for {
        MapExtractor(sentence) <- sentences
      } yield {
        val ListExtractor(deps) = sentence("deps")
        val ListExtractor(tokens) = sentence("tokens")
        val ListExtractor(mentions) = sentence("mentions")
        val ListExtractor(eventCandidates) = sentence("eventCandidates")

        val wolfeTokens = tokens.map(t => {
          val MapExtractor(token) = t
          val charOffsets = CharOffsets(token("begin").toString.toInt, token("end").toString.toInt)
          // ACHTUNG!!! IT'S A STEM, NOT A LEMMA (Token case class takes a lemma, not a stem, but you can use it as a stem)
          Token(token("word").toString, charOffsets, token("pos").toString, token("stem").toString)
        }).sortBy(_.offsets.start).toArray

        val wolfeMentions = mentions.map(m => {
          val MapExtractor(mention) = m
          EntityMention(mention("label").toString, mention("begin").toString.toInt, mention("end").toString.toInt)
        }).toIndexedSeq

        val wolfeDependencyTreeArcs = deps.map(d => {
          val MapExtractor(dep) = d
          (dep("mod").toString.toInt, dep("head").toString.toInt, dep("label").toString)
        }).sortBy(_._1).toArray

        val wolfeDependencyTree = new DependencyTree(wolfeTokens, wolfeDependencyTreeArcs)

        val wolfeEventCandidates = eventCandidates.map(e => {
          val MapExtractor(event) = e
          val entityMention = EntityMention(event("gold").toString,
                                            event("begin").toString.toInt,
                                            event("end").toString.toInt)
          val ListExtractor(args) = event("arguments")
          val arguments = args.map(a => {
            val MapExtractor(arg) = a
            RoleMention("argument", EntityMention(arg("gold").toString, arg("begin").toString.toInt, arg("end").toString.toInt))
          }).toArray
          EventMention("eventCandidate", entityMention, arguments)
        }).toIndexedSeq

        Sentence(wolfeTokens, syntax = new SyntaxAnnotation(ConstituentTree.empty, wolfeDependencyTree), ie = new IEAnnotation(wolfeMentions, IndexedSeq(), wolfeEventCandidates, semanticFrames = null))
      }
    Document(txt, wolfeSentences.toIndexedSeq)
  }
}

object LoadBioNLPTest extends App {
  val pathToBioNLP = args.lift(0).getOrElse("wolfe-nlp/data/bionlp-train")
  val jsonFiles = new File(pathToBioNLP).listFiles(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name.endsWith(".json")
  }).toList

  println(jsonFiles.head)
  val doc = LoadBioNLP.jsonFileToWolfeDoc(jsonFiles.head)
  println(doc)
}