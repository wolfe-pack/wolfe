package ml.wolfe.nlp.io

import ml.wolfe.nlp.{Sentence, CharOffsets, Token}

/**
 * @author Sebastian Riedel
 */
object JsonIO {

  def main(args: Array[String]) {
    import org.json4s._
    import org.json4s.native.Serialization
    import org.json4s.native.Serialization.{read, write}
    implicit val formats = Serialization.formats(NoTypeHints)
    val token = Token("the", CharOffsets(0, 3))
    val sentence = Sentence(IndexedSeq(token))
    val ser = write(token)

    println(ser)

    val deser = read[Token](ser)
    println(deser)
    println(write(sentence))
  }
}
