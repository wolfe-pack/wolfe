package ml.wolfe.nlp.io

import java.io.{FilenameFilter, File}

import com.fasterxml.jackson.databind.ObjectMapper
import ml.wolfe.nlp._

import scala.util.parsing.json.JSON

/**
 * @author rockt
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
          val charOffsets = CharOffsets(token("begin").toString.toDouble.toInt, token("end").toString.toDouble.toInt)
          Token(token("word").toString, charOffsets, token("pos").toString)
        }).sortBy(_.offsets.start).toArray

        val wolfeMentions = mentions.map(m => {
          val MapExtractor(mention) = m
          EntityMention(mention("label").toString, mention("begin").toString.toDouble.toInt, mention("end").toString.toDouble.toInt)
        })

        //println(eventCandidates.mkString("\n"))

        //todo: add deps
        //todo: add (candidate) events

        Sentence(wolfeTokens, ie = new IEAnnotation(wolfeMentions, Nil, Nil))
      }

    Document(txt, wolfeSentences)
  }
}

object LoadBioNLPTest extends App {
  val pathToBioNLP = args.lift(0).getOrElse("wolfe-nlp/data/bionlp")
  val jsonFiles = new File(pathToBioNLP).listFiles(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name.endsWith(".json")
  }).toList

  val doc = LoadBioNLP.jsonFileToWolfeDoc(jsonFiles.head)
  println(doc)
}