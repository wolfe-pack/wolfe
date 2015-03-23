package ml.wolfe.nlp.io

import java.nio.file.{Paths, Files}

import ml.wolfe.nlp._
import ml.wolfe.nlp.discourse.{DiscourseRelation, DiscourseArgument}
import ml.wolfe.nlp.syntax.{Arc, DependencyTree, ConstituentTree}
import org.json4s.JsonAST.{JInt, JObject, JString}
import org.json4s._
import org.json4s.native.JsonMethods._
import java.nio.charset.StandardCharsets

import scala.io.Source

/**
 * @author matko
 */


object CoNLL2015DiscourseReader {

  implicit val formats = DefaultFormats

  case class JSONDocument(sentences: List[JSONSentence])
  case class JSONSentence(dependencies: List[List[String]], parsetree: String, words: List[List[JValue]])
  case class JSONDiscourseRelation(Arg1: JSONDiscourseArgument, Arg2: JSONDiscourseArgument, Connective: JSONDiscourseArgument, DocID: String, ID: Int, Sense: List[String], Type: String)
  case class JSONDiscourseArgument(CharacterSpanList: List[List[Int]] , RawText: String, TokenList: List[List[Int]])

  // load relations and group them by documentID
  def loadDiscourseRelations(filename: String): Map[String, List[JSONDiscourseRelation]] = {
    val lines_data = Source.fromFile(filename).getLines()
    val data = lines_data.map{ line_data =>
      val json = parse(line_data)
      json.extract[JSONDiscourseRelation]
    }.toList
    data.groupBy(_.DocID)
  }



  def loadData(dataDirectory: String): Iterable[Document] = {
    val doc_relations = loadDiscourseRelations(dataDirectory + "pdtb-data.json")
    val lines_parses = Source.fromFile(dataDirectory + "pdtb-parses.json").getLines()

    val content = lines_parses.mkString
    val json_parses = parse(content)
    val parses = json_parses.extract[Map[String, JSONDocument]]


    val documents = parses.map { document =>
      val docID = document._1

      val sentences = document._2.sentences.map { sentence =>

        val tokens = sentence.words.map { word =>
          val JString(token) = word(0)
          val JObject(properties) = word(1)
          val JInt(offset_start) = properties(0)._2
          val JInt(offset_end) = properties(1)._2
          // LINKERS NOT USED
          //val JArray(linkers) = properties(2)._2
          val JString(posTag) = properties(3)._2
          val offsets = CharOffsets(offset_start.toInt, offset_end.toInt)
          Token(token, offsets, posTag)
        }.toIndexedSeq

        val arcs = sentence.dependencies.map { dependency =>
          val label = dependency(0)
          val child = dependency(1).splitAt(dependency(1).lastIndexOf("-")+1)._2.toInt - 1
          val head = dependency(2).splitAt(dependency(2).lastIndexOf("-")+1)._2.toInt - 1
          Arc(child, head, Some(label))
        }.filterNot(_.label.get == "root").toSeq

        val parse_tree = "(ROOT" + sentence.parsetree.drop(1).dropRight(2) + ")"
        val cons_tree = ConstituentTreeFactory.stringToTree(parse_tree).getOrElse(ConstituentTree.empty)
        val dep_tree = DependencyTree(tokens, arcs)

        Sentence(tokens, new SyntaxAnnotation(cons_tree, dep_tree))
      }

      val filename = dataDirectory + "raw/" + docID
      val text = Source.fromFile(filename, "ISO-8859-1").getLines().mkString("\n")

      val discourse = doc_relations.get(docID) match {
        case Some(relations) =>
          val rels = relations.map{ relation =>
            def toWolfeDiscourseArgument(x: JSONDiscourseArgument): DiscourseArgument = {
              val tokens = x.TokenList.map(tok => (tok(3), tok(4))).toSeq
              val offsets = x.CharacterSpanList.map(span => CharOffsets(span(0), span(1)))
              DiscourseArgument(x.RawText, offsets, tokens)
            }
            val arg1 = toWolfeDiscourseArgument(relation.Arg1)
            val arg2 = toWolfeDiscourseArgument(relation.Arg2)
            val connective = toWolfeDiscourseArgument(relation.Connective)
            DiscourseRelation(arg1, arg2, connective, relation.ID.toString, relation.Sense, relation.Type)
          }.toSeq
          DiscourseAnnotation(rels)
        case None => DiscourseAnnotation.empty
      }
      Document(text, sentences.toIndexedSeq, filename = Some(filename), id = Some(docID), discourse = discourse)
    }
    documents
  }


}

object CoNLL2015DiscourseWriter {

  case class JSONDiscourseRelation(Arg1: JSONDiscourseArgument, Arg2: JSONDiscourseArgument, Connective: JSONDiscourseArgument, DocID: String, ID: Int, Sense: List[String], Type: String)
  case class JSONDiscourseArgument(CharacterSpanList: List[List[Int]] , RawText: String, TokenList: List[List[Int]])

  def writeDocumentsToJSON(documents: Iterable[Document], filename: String) = {
    val sb = new StringBuilder
    documents.toIterator.foreach{ document =>
      val jSONDiscourseRelations = documentToJSONDiscourseRelations(document)
      import org.json4s.native.Serialization.write
      implicit val formats = org.json4s.native.Serialization.formats(NoTypeHints)
      jSONDiscourseRelations.foreach(relation => sb.append(write(relation) + "\n"))
    }
    scala.tools.nsc.io.File(filename).writeAll(sb.toString)
  }

  def documentToJSONDiscourseRelations(document: Document): Seq[JSONDiscourseRelation] = {
    // pre-calculate document sentence lengths, needed for token offsets
    var counter = 0
    val sentence_lengths = document.sentences.map{ sentence =>
      counter += sentence.size
      counter
    }
    val ret = document.discourse.relations.map { relation =>

      def charOffsetsToSpanList(x: List[CharOffsets]) = x.map{ offset => List(offset.start, offset.end)}
      def rewriteTokens(sentenceTokenPairs: Seq[(Int, Int)]) = {
        sentenceTokenPairs.map { pair =>
          val sentenceID = pair._1
          val tokenID = pair._2
          val offsets = document.sentences(sentenceID).tokens(tokenID).offsets
          val tokenOffset = sentence_lengths(if (sentenceID - 1 >= 0) sentenceID - 1 else 0) + tokenID
          List(offsets.start, offsets.end, tokenOffset, sentenceID, tokenID)
        }.toList
      }

      val arg1_characterspanlist = charOffsetsToSpanList(relation.arg1.charOffsets)
      val arg2_characterspanlist = charOffsetsToSpanList(relation.arg2.charOffsets)
      val connective_characterspanlist = charOffsetsToSpanList(relation.connective.charOffsets)

      val arg1_tokens = rewriteTokens(relation.arg1.tokens)
      val arg2_tokens = rewriteTokens(relation.arg2.tokens)
      val connective_tokens = rewriteTokens(relation.connective.tokens)

      val arg1 = JSONDiscourseArgument(arg1_characterspanlist, relation.arg1.text, arg1_tokens)
      val arg2 = JSONDiscourseArgument(arg2_characterspanlist, relation.arg2.text, arg2_tokens)
      val connective = JSONDiscourseArgument(connective_characterspanlist, relation.connective.text, connective_tokens)

      JSONDiscourseRelation(arg1, arg2, connective, document.id.get, relation.id.toInt, relation.sense, relation.typ)
    }
    ret

  }
}

object CoNLL2015TestReadWrite extends App {
  println("Initiating reader.")
  val output = CoNLL2015DiscourseReader.loadData("/Users/matko/workspace/conll2015/data/conll15st-train-dev/conll15st_data/conll15-st-03-04-15-train/")
  println("Reading DONE. Initiating writing.")
  CoNLL2015DiscourseWriter.writeDocumentsToJSON(output, "output.json")
  println("Writing DONE.")
}