package ml.wolfe.nlp

import ml.wolfe.nlp.syntax.ConstituentTree

/**
 * @author riedel
 */
class NavigableDocument(val docToNavigate: Document) {

  lazy val char2tokens = docToNavigate.sentences.flatMap {
    _.tokens.zipWithIndex.flatMap {
      case (t, i) => t.offsets.range.map(_ ->(t, i))
    }
  }.toMap

  lazy val char2sentence = docToNavigate.sentences.zipWithIndex.flatMap {
    case (s, i) => s.offsets.range.map(_ ->(s, i))
  }.toMap

  lazy val char2trees = docToNavigate.sentences.flatMap {
    s => s.syntax.constituency.getOrElse(ConstituentTree.empty).breadthFirstSearch.flatMap {
      t =>
        val offsets = CharOffsets(s.tokens(t.start).offsets.start, s.tokens(t.end).offsets.end)
        offsets.range map (_ -> t)
    }
  }.groupBy(_._1).mapValues(_.map(_._2))


  implicit class NavigableCharOffsets(offsets: CharOffsets) {
    def tokens = offsets.range.collect(char2tokens andThen (_._1)).distinct
    def sentences = tokens.map(_.sentence).distinct
  }

  implicit class NavigableToken(token: Token) {
    def document = docToNavigate

    def sentence = char2sentence(token.offsets.start)._1

    def index = char2tokens(token.offsets.start)._2

    def trees = char2trees(token.offsets.start)
  }

  implicit class NavigableSentence(sentence: Sentence) {
    def document = docToNavigate

    def index = char2sentence(sentence.offsets.start)._2
    def next = document.sentences.lift(index + 1)
    def prev = document.sentences.lift(index - 1)
  }


}


