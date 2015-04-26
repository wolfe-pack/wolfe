package ml.wolfe.nlp.io

import ml.wolfe.nlp._
import scala.collection.mutable.ArrayBuffer
import ml.wolfe.nlp.ie.EntityMention

/**
 * Created by narad on 2/27/15.
 */
class GoogleNewsReader(filename: String, verbose: Boolean = false) extends Iterable[Document] {
  val chunks = new ChunkReader(filename)
  val proto = new ProtoReader(ldelim = "{", rdelim = "}")

  def iterator: Iterator[Document] = {
    chunks.view.zipWithIndex.map { case(chunk, chunkIdx) =>
      if (verbose) print("\rReading...%d.".format(chunkIdx))
      parseProtoDocument("top {\n" + chunk + "\n}")
    }.iterator
  }

  def parseProtoDocument(chunk: String): Document = {
    val parse = proto.parse(chunk)
    assert(parse.size == 1, "There should only be one top level node per document.")
    val sentences = parseProtoSentences(parse.head)
    val entities = parseProtoEntities(parse.head)
    Document(source = chunk, sentences = mergeSentencesAndEntities(sentences, entities))
  }

  def mergeSentencesAndEntities(sentences: IndexedSeq[Sentence], entities: IndexedSeq[EntityMention]): IndexedSeq[Sentence] = {
    var tcount = 0
    sentences.map { s =>
      val sc = s.copy(ie = IEAnnotation(entityMentions = entities.collect { case e if e.start > tcount && e.start < tcount + s.size =>
        e.copy(start = e.start - tcount, end = e.end - tcount + 1)
      }))
      tcount += s.size
      sc
    }
  }


  def parseProtoSentences(node: ProtoNode): IndexedSeq[Sentence] = {
    val sentences = new ArrayBuffer[Sentence]
    val tokens = new ArrayBuffer[Token]
    for (child <- node.search) {
      child match {
        case parent: ProtoParentNode => {
          if (parent.field == "token") {
            val (token, break) = parseProtoToken(parent)
            tokens += token
            if (break) {
              sentences += Sentence(tokens.toIndexedSeq)
              tokens.clear()
            }
          }
        }
        case _ =>
      }
    }
    if (tokens.nonEmpty) sentences += Sentence(tokens.toIndexedSeq)
    sentences.toIndexedSeq
  }

  def parseProtoEntities(node: ProtoNode): IndexedSeq[EntityMention] = {
    node.search.collect{ case x: ProtoParentNode => x }.collect { case n if n.field == "entity" =>
      val values = n.search.collect { case x: ProtoValueNode => x}
      val children = n.search.collect { case x: ProtoParentNode => x}
      val ename = values.find(_.field == "name").getOrElse(ProtoValueNode("name", "None")).value
      val etype = values.find(_.field == "entity_type").getOrElse(ProtoValueNode("name", "None")).value
      val profile = n.value.collect { case x: ProtoParentNode => x}.collect { case p if p.field == "profile" =>
        val identifiers = p.value.collect { case x: ProtoParentNode => x}.collect { case i if i.field == "identifier" =>
          val pvalues = i.value.collect { case x: ProtoValueNode => x}
          val domain = pvalues.find(_.field == "domain").get.value
          val id = pvalues.find(_.field == "id").get.value
          domain -> id
        }
        identifiers
      }.flatten.toMap
      val mid = profile.getOrElse("FREEBASE_MID", "None")
      val mentions = n.search.collect{ case x: ProtoParentNode => x}.collect { case m if m.field == "mention" =>
        m.value.collect { case x: ProtoParentNode => x}.collect { case p if p.field == "phrase" =>
          val pchildren = p.value.collect { case x: ProtoValueNode => x }
          val start = pchildren.find(_.field == "start").get.value.toInt
          val end = pchildren.find(_.field == "end").get.value.toInt
          EntityMention(label = etype, start = start, end = end, id = mid)
        }
      }.flatten
      mentions
    }.flatten.toIndexedSeq
  }

  def parseProtoToken(node: ProtoParentNode): (Token, Boolean) = {
    val nodes = node.search.collect{ case x: ProtoValueNode => x }
    val word = nodes.find(_.field == "word").get.value
    val start = nodes.find(_.field == "start").get.value.toInt
    val end = nodes.find(_.field == "end").get.value.toInt
    val tag = nodes.find(_.field == "tag").get.value
    val boundary = nodes.find(_.field == "break_level").get.value
    (Token(word = word, offsets = CharOffsets(start, end), posTag = tag), boundary == "SENTENCE_BREAK")
  }

}

object GoogleNewsReader {

  def main(args: Array[String]): Unit = {
    for (d <- new GoogleNewsReader(args(0), args.mkString(" ").contains("--verbose TRUE"))) {
      val i = 0 // blah
    }
    println("Finished.")
  }
}


















//
//        map { e =>
//        val si = s.tokens.indexWhere(t => e.start == t.offsets.start)
//        val ei = s.tokens.indexWhere(t => e.end == t.offsets.end)
//        e.copy(start = si, end = ei + 1)
//      }.filter(e => e.start > -1 && e.end > -1)))
//    }
//  }

//
//  def iterator: Iterator[Document] = {
//    chunks.iterator.map(c => parseDocument(c))
//  }
//
//  def parseDocument(str: String): Document = {
////    val mstr = "{" + str + "}"
////    val sexp = new SExpressionReader(ldelim = "{", rdelim = "}")
////    for (s <- sexp.readFromString(mstr)) println("SEXP: " + s)
//    ???
//  }

  /*
  def parseDocument(str: String): Document = {
  //  println("str: " + str
    val tokBuffer = new ArrayBuffer[Token]
    val sentBuffer = new ArrayBuffer[Sentence]
    val TOKEN_PATTERN = """token \{ word: \"([^\"]+)\" start: ([0-9]+) end: ([0-9]+) [^\}]+ tag: \"([^\"]+)\" ([^\}]+) break_level: ([^\}]+) \}""".r
    (TOKEN_PATTERN findAllIn str.replaceAll("\n", " ").replaceAll(" +", " ")).matchData foreach { m =>
      tokBuffer += Token(word = m.group(1), offsets = CharOffsets(m.group(2).toInt, m.group(3).toInt), posTag = m.group(4))
      if (m.group(6) == "SENTENCE_BREAK") {
        sentBuffer += Sentence(tokens = tokBuffer.toIndexedSeq)
        tokBuffer.clear()
      }
    }
    if (tokBuffer.nonEmpty) sentBuffer += Sentence(tokens = tokBuffer.toIndexedSeq)
    println("sentences = " + sentBuffer.size)
    parseEntity(str)
    new Document(source = str, sentences = sentBuffer.toIndexedSeq)
  }

  def parseToken(str: String): Token = {
   null
  }

  def parseSemanticNode = ???

  def parseEntity(str: String): Array[EntityMention] = {
    println("entity parsing")
    val ENTITY_CHUNK_PATTERN = """entity \{.+? <BR>\}""".r
    val ENTITY_PATTERN = """entity \{ name: \"([^\"]+)\" entity_type: \"([^\"]+)\" .* \} entity""".r
    val mstr = str.replaceAll("\n", "<BR>").replaceAll(" +", " ")
    println(mstr)
    (ENTITY_CHUNK_PATTERN findAllIn mstr).matchData.map { m =>
      println("CHUNK " + m + "\n")
      EntityMention(label = "", start = 0, end = 0, id = "")
    }.toArray
  }
       */
  /*

  entity {
  name: "National Rifle Association"
  entity_type: "ORG"
  entity_type_probability: 0
  entity_type_probability: 0
  entity_type_probability: 1
  entity_type_probability: 0
  mention {
    phrase {
      start: 86
      end: 88
      head: 88
    }
    type: NAM
    kind: REFERENTIAL
  }
  profile {


  token {
  word: "to"
  start: 178
  end: 179
  head: 33
  tag: "TO"
  category: "PRT"
  label: "aux"
  break_level: SPACE_BREAK
   */

