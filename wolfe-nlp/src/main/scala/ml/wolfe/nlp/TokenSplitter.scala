package ml.wolfe.nlp

import scala.collection.mutable.ArrayBuffer

/**
 * @author Sebastian Riedel
 */
object TokenSplitter extends (Document => Document) {

  def apply(doc: Document) = {
    //go through all tokens and split the token at white space
    val text = doc.source
    val tokens = new ArrayBuffer[Token]
    tokens.sizeHint(1000)

    def split(token: Token) = {
      tokens.clear()
      val start = token.offsets.start
      val end = token.offsets.end
      var offset = token.offsets.start
      val buffer = new StringBuilder
      while (offset < end) {
        while (offset < end && Character.isWhitespace(text(offset))) { offset += 1 }
        val newTokenStart = offset
        buffer.clear()
        while (offset < end && !Character.isWhitespace(text(offset))) {
          buffer.append(text(offset))
          offset += 1
        }
        //tokens += Token(text.substring(newTokenStart,offset), CharOffsets(newTokenStart, offset))
        tokens += Token(buffer.toString(), CharOffsets(newTokenStart, offset))

      }
      Seq.empty ++ tokens
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

  val sentenceEnds = Set('.','?', ';')


  def apply(doc: Document) = {
    val text = doc.source
    val buffer = new StringBuilder
    val tokens = new ArrayBuffer[Token]
    tokens.sizeHint(50)

    def split(sentence: Sentence) = {
      val sentences = new ArrayBuffer[Sentence]
      for (token <- sentence.tokens) {
        var start = token.offsets.start
        val end = token.offsets.end
        var offset = token.offsets.start
        while (offset < end) {
          while (offset < end && !sentenceEnds(text(offset))) {
            buffer.append(text(offset))
            offset += 1
          }
          if (offset < end && sentenceEnds(text(offset))) {
            //create a new token until here.
            val newToken = Token(buffer.toString(),CharOffsets(start,offset))
            val punctToken = Token(text(offset).toString,CharOffsets(offset,offset+1))
            tokens += newToken
            tokens += punctToken
            sentences += Sentence(Seq.empty ++ tokens)
            tokens.clear()
            offset += 1
            while (offset < end && Character.isWhitespace(text(offset))) { offset += 1 }
            start = offset
            buffer.clear()
          } else {
            tokens += Token(buffer.toString(),CharOffsets(start,offset))
            buffer.clear()
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

  def main(args: Array[String]) {
    println(SentenceSplitter("This is Wolfe. What are you?"))
  }

}