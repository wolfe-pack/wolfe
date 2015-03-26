package ml.wolfe.nlp

/**
 * @author riedel
 */
class NavigableDocument(val doc: Document) {

  lazy val char2tokens = doc.sentences.flatMap {
    _.tokens.zipWithIndex.flatMap {
      case (t, i) => t.offsets.range.map(_ ->(t, i))
    }
  }.toMap

  lazy val char2sentence = doc.sentences.zipWithIndex.flatMap {
    case (s, i) => s.offsets.range.map(_ ->(s, i))
  }.toMap

  lazy val char2trees = doc.sentences.flatMap {
    s => s.syntax.tree.breadthFirstSearch.flatMap {
      t =>
        val offsets = CharOffsets(s.tokens(t.start).offsets.start, s.tokens(t.end).offsets.end)
        offsets.range map (_ -> t)
    }
  }.groupBy(_._1).mapValues(_.map(_._2))


  implicit class NavigableCharOffsets(offsets: CharOffsets) {
    def tokens = offsets.range.collect(char2tokens andThen (_._1)).distinct
  }

  implicit class NavigableToken(token: Token) {
    def document = doc

    def sentence = char2sentence(token.offsets.start)._1

    def index = char2tokens(token.offsets.start)._2

    def trees = char2trees(token.offsets.start)
  }

  implicit class NavigableSentence(sentence: Sentence) {
    def document = doc

    def index = char2sentence(sentence.offsets.start)._2
  }


}


