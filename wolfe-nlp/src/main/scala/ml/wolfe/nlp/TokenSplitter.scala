package ml.wolfe.nlp

import scala.collection.mutable.ArrayBuffer

/**
 * @author Sebastian Riedel
 */
object TokenSplitter extends (Document => Document) {

  def apply(doc: Document) = {
    //go through all tokens and split the token at white space
    val text = doc.source

    def split(token: Token) = {
      val start = token.offsets.start
      val end = token.offsets.end
      var offset = token.offsets.start
      val tokens = new ArrayBuffer[Token]
      while (offset < end) {
        while (Character.isWhitespace(text(offset))) { offset += 1 }
        val newTokenStart = offset
        while (offset < end && !Character.isWhitespace(text(offset))) { offset += 1 }
        tokens += Token(text.substring(newTokenStart,offset), CharOffsets(newTokenStart, offset))
      }
      tokens
    }
    doc.copy(sentences = doc.sentences.map(s => s.copy(tokens = s.tokens.flatMap(split))))
  }

  def main(args: Array[String]) {
    val tokenized = TokenSplitter("This is Wolfe. What are you? Blah blub")
    println(tokenized)
    val split = SentenceSplitter(tokenized)
    println(split.toText)
  }
}

object SentenceSplitter extends (Document => Document) {

  val sentenceEnds = Set('.','?')


  def apply(doc: Document) = {
    val text = doc.source

    def split(sentence: Sentence) = {
      val sentences = new ArrayBuffer[Sentence]
      var tokens = new ArrayBuffer[Token]
      for (token <- sentence.tokens) {
        var start = token.offsets.start
        val end = token.offsets.end
        var offset = token.offsets.start
        while (offset < end) {
          while (offset < end && !sentenceEnds(text(offset))) {
            offset += 1
          }
          if (offset < end && sentenceEnds(text(offset))) {
            //create a new token until here.
            val newToken = Token(text.substring(start,offset),CharOffsets(start,offset))
            val punctToken = Token(text(offset).toString,CharOffsets(offset,offset+1))
            offset += 1
            tokens += newToken
            tokens += punctToken
            sentences += Sentence(tokens.toSeq)
            start = offset
            tokens = new ArrayBuffer[Token]
          } else {
            tokens += Token(text.substring(start,offset),CharOffsets(start,offset))
          }
        }
      }
      //turn last set of tokens into new sentence
      if (tokens.size > 0) {
        sentences += Sentence(tokens)
      }
      sentences
    }
    doc.copy(sentences = doc.sentences.flatMap(split))
  }
}