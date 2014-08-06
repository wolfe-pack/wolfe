package ml.wolfe.nlp

import edu.arizona.sista.processors.{Sentence => SISTASent}
import edu.arizona.sista.processors.struct.Tree

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
    val ctree = toWolfeConstituentTree(sentence)
    val dtree = toWolfeDependencyTree(sentence)
    Sentence(tokens, syntax = new SyntaxAnnotation(tree = ctree, dependencies = dtree))
  }

  def toWolfeConstituentTree(sent: SISTASent): ConstituentTree = {
    sent.syntacticTree match {
      case Some(tree) => treeToTree(tree)
      case _=> ConstituentTree.empty
    }
  }

  def toWolfeDependencyTree(sent: SISTASent): DependencyTree = {
    val dt = new DependencyTree(sent.dependencies.get.outgoingEdges.zipWithIndex.flatMap { case(x, i) => x.map { y => (i, y._1, y._2) }})
//    println(dt.toString())
    dt
  }

  def treeToTree(tree: Tree[String]): ConstituentTree = {
    if (tree.isPreTerminal) {
      new ConstituentTree(new PreterminalNode(label = tree.value, word = tree.children.get.head.value))
    }
    else {
      new ConstituentTree(new NonterminalNode(label = tree.value, head = tree.head), tree.children.get.map(treeToTree(_)))
    }
  }

}
