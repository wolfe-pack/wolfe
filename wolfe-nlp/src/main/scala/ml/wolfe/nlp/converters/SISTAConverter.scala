package ml.wolfe.nlp.converters

import edu.arizona.sista.processors.{CorefChains => SISTACorefChains, Sentence => SISTASent}
import edu.arizona.sista.struct.{Tree => SistaTree}
import ml.wolfe.nlp._
import ml.wolfe.nlp.ie.{EntityMention, CorefMention}
import ml.wolfe.nlp.syntax._

import scala.collection.mutable.ArrayBuffer

/**
 * Conversion code for the SISTA processor package.
 * @author Sebastian Riedel
 */
object SISTAConverter {

  def toWolfeCoreference(chains: SISTACorefChains): Seq[CorefMention] = {
    val ret = for ((chain, clusterID) <- chains.getChains.zipWithIndex; mention <- chain) yield {
      CorefMention(clusterID, mention.sentenceIndex, mention.startOffset, mention.endOffset, mention.headIndex)
    }
    ret.toSeq
  }

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

  def toFullWolfeSentence(sent: SISTASent): Sentence = {
    val tokens = for (i <- 0 until sent.size) yield toWolfeToken(i, sent)
    val ctree = toWolfeConstituentTree(sent)
    val dtree = toWolfeDependencyTree(sent)
    val entities = toWolfeEntities(sent)
    Sentence(tokens,
             ie = new IEAnnotation(entityMentions = entities, relationMentions = null, eventMentions = null, semanticFrames = null),
             syntax = new SyntaxAnnotation(tree = ctree, dependencies = dtree))
  }

  def toWolfeConstituentTree(sent: SISTASent): ConstituentTree = {
    sent.syntacticTree match {
      case Some(tree) => treeToTree(tree)
      case _=> ConstituentTree.empty
    }
  }

  def toWolfeDependencyTree(sent: SISTASent): DependencyTree = {
    val tokens = for (i <- 0 until sent.size) yield toWolfeToken(i, sent)
    val dt = new DependencyTree(tokens, sent.dependencies.get.outgoingEdges.zipWithIndex.flatMap { case(x, i) => x.map { y => Arc(i, y._1, Some(y._2)) }})
//    println(dt.toString())
    dt
  }

  def treeToTree(tree: SistaTree, leftMost: Int = 0): ConstituentTree = {
    if (tree.isPreTerminal) {
      new ConstituentTree(new PreterminalNode(start = leftMost, end = leftMost + 1, label = tree.value, word = tree.children.get.head.value))
    }
    else {
      var tmpLeftMost = leftMost
      val children = tree.children.get.map { t =>
        val child = treeToTree(t, leftMost = tmpLeftMost)
        tmpLeftMost = child.end
        child
      }
      val rightMost = children.last.end
      new ConstituentTree(new NonterminalNode(start = leftMost, end = rightMost, label = tree.value),
                          children = children.toList)
    }
  }

  def toWolfeEntities(sent: SISTASent): IndexedSeq[EntityMention] = {
    sent.entities match {
      case Some(entities) => {
        var lastIndex = -1
        var lastSymbol = "O"
        val stored = new ArrayBuffer[EntityMention]
        entities.zipWithIndex.foreach { case(label, idx) =>
          if (idx == 0 && label != "O") lastIndex = 0
          else if (label != lastSymbol && lastSymbol != "O") {
            stored += new EntityMention(lastSymbol, lastIndex, idx)
            if (label != "O") lastIndex = idx
          }
          else if (label != lastSymbol && lastSymbol == "O") {
            lastIndex = idx
          }
          else if (label != "O" && idx+1 == entities.size) {
            stored += new EntityMention(label, lastIndex, idx)
          }
          lastSymbol = label
        }
        stored.toIndexedSeq
      }
      case _ => IndexedSeq()
    }
  }
}
