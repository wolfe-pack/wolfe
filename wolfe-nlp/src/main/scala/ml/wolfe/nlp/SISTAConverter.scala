package ml.wolfe.nlp

import edu.arizona.sista.processors.{Sentence => SISTASent}

/**
 * Conversion code for the SISTA processor package.
 * @author Sebastian Riedel
 */
object SISTAConverter {

  def toWolfeSentence(sentence: SISTASent): Sentence = {
    val tokens = for (i <- 0 until sentence.size) yield toWolfeToken(i, sentence)
    Sentence(tokens)
  }

  def toWolfeToken(index: Int, sent: SISTASent): Token = {
    def asOption[T](array:Option[Array[T]]):Option[T] = array.map(_.view.apply(index))
    def asNull[T <: AnyRef](array:Option[Array[T]]):T = asOption(array).getOrElse(null.asInstanceOf[T])
    Token(
      word = sent.words(index),
      offsets = CharOffsets(sent.startOffsets(index),sent.endOffsets(index)),
      posTag = asNull(sent.tags),
      lemma = asNull(sent.lemmas)
    )
  }

  def toFullWolfeSentence(sentence: SISTASent): Sentence = {
    val tokens = for (i <- 0 until sentence.size) yield toWolfeToken(i, sentence)
    val tree = toWolfeDependencyTree(sentence)
    Sentence(tokens, syntax = new SyntaxAnnotation(tree = null, dependencies = tree))
  }

  def toWolfeDependencyTree(sent: SISTASent): DependencyTree = {
    val dt = new DependencyTree(sent.dependencies.get.outgoingEdges.zipWithIndex.flatMap { case(x, i) => x.map { y => (i, y._1, y._2) }})
    println(dt.toString())
    dt
  }

}
