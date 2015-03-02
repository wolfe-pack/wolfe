package ml.wolfe.nlp.io

import ml.wolfe.nlp._
import scala.collection.mutable.ArrayBuffer

/**
 * Created by narad on 2/27/15.
 */
class GoogleNewsReader(filename: String) extends Iterable[Document] {
  val chunks = new ChunkReader(filename)
  val proto = new ProtoReader(ldelim = "{", rdelim = "}")

  def iterator: Iterator[Document] = {
    chunks.view.map(c => parseProtoDocument(c))
    ???
  }

  def parseProtoDocument(chunk: String): Document = {
    val node = proto.parse(chunk)
    //println(node.)
    ???
  }

}

object GoogleNewsReader {

  def main(args: Array[String]): Unit = {
    for (d <- new GoogleNewsReader(args(0))) {
      val b = d
      // println(d)
    }
    println("Finished.")
  }
}


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

