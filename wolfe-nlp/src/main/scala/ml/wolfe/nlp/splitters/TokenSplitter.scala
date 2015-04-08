package ml.wolfe.nlp.splitters

import scala.collection.mutable.ArrayBuffer
import ml.wolfe.nlp.structures._

import scala.language.higherKinds


/**
 * @author Sebastian Riedel
 */
object TokenSplitter {

  val oneCharTokens = Set(',', '"', '\'', '(', ')', '/', '[', ']')

  def apply[D <: DocumentLike[_ <: SentenceLike[_ <: TokenLike]]](doc: D) = {
    //go through all tokens and split the token at white space
    val text = doc.source
    val tokens = new ArrayBuffer[ml.wolfe.nlp.structures.Token]
    tokens.sizeHint(1000)

    def split(token: TokenLike): IndexedSeq[ml.wolfe.nlp.structures.TokenLike] = {
      tokens.clear()
      val end = token.offsets.end
      var offset = token.offsets.start
      val buffer = new StringBuilder
      while (offset < end) {
        while (offset < end && Character.isWhitespace(text(offset))) {offset += 1}
        val newTokenStart = if (offset < end && oneCharTokens(text(offset))) {
          tokens += ml.wolfe.nlp.structures.Token(text(offset).toString, CharOffsets(offset, offset + 1))
          offset + 1
        } else offset
        buffer.clear()
        while (offset < end && !Character.isWhitespace(text(offset))) {
          buffer.append(text(offset))
          offset += 1
        }
        if (oneCharTokens(text(offset - 1))) {
          tokens += ml.wolfe.nlp.structures.Token(buffer.toString().dropRight(1), CharOffsets(newTokenStart, offset - 1))
          tokens += ml.wolfe.nlp.structures.Token(text(offset - 1).toString, CharOffsets(offset - 1, offset))
        } else {
          //tokens += Token(text.substring(newTokenStart,offset), CharOffsets(newTokenStart, offset))
          tokens += ml.wolfe.nlp.structures.Token(buffer.toString(), CharOffsets(newTokenStart, offset))
        }

      }
      IndexedSeq.empty ++ tokens
    }
    doc.map({ x: SentenceLike[_ <: TokenLike] => x.flatMap({ y: TokenLike => split(y) }) })
  }
}


object SentenceSplitter {

  val sentenceEnds = Set('.', '?', ';')



  def apply[D <: DocumentLike[_ <: SentenceLike[_ <: TokenLike]]](doc: D)= {
    val text = doc.source
    val buffer = new StringBuilder
    val tokens = new ArrayBuffer[ml.wolfe.nlp.structures.Token]
    tokens.sizeHint(50)

    def split[S <: SentenceLike[_ <: TokenLike]](sentence: S) : IndexedSeq[ml.wolfe.nlp.structures.SentenceLike[ml.wolfe.nlp.structures.TokenLike]] = {
      val sentences = new ArrayBuffer[ml.wolfe.nlp.structures.SentenceLike[ml.wolfe.nlp.structures.TokenLike]]
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
            val newToken = ml.wolfe.nlp.structures.Token(buffer.toString(), CharOffsets(start, offset))
            val punctToken = ml.wolfe.nlp.structures.Token(text(offset).toString, CharOffsets(offset, offset + 1))
            tokens += newToken
            tokens += punctToken
            sentences += ml.wolfe.nlp.structures.Sentence(tokens.toIndexedSeq)
            tokens.clear()
            offset += 1
            while (offset < end && Character.isWhitespace(text(offset))) { offset += 1 }
            start = offset
            buffer.clear()
          } else {
            tokens += ml.wolfe.nlp.structures.Token(buffer.toString(), CharOffsets(start, offset))
            buffer.clear()
          }
        }
      }
      //turn last set of tokens into new sentence
      if (tokens.size > 0) {
        sentences += ml.wolfe.nlp.structures.Sentence(tokens.toIndexedSeq)
      }
      sentences
    }

    doc flatMap split _
  }

}
