package ml.wolfe.nlp

import breeze.linalg.SparseVector
import ml.wolfe.nlp.discourse.DiscourseRelation
import ml.wolfe.nlp.semantics.{SemanticFrame}
import ml.wolfe.nlp.ie.{EventMention, RelationMention, EntityMention}
import ml.wolfe.nlp.syntax.{DependencyTree, ConstituentTree}

/**
 * A mention of a named entity.
 * @param entityMentions sequence of entity mentions found in the sentence.
 * @param relationMentions sequence of relation mentions found in the sentence.
 * @param eventMentions sequence of event mentions found in the sentence.
 */
case class IEAnnotation(entityMentions: IndexedSeq[EntityMention]=IndexedSeq.empty,
                        relationMentions: IndexedSeq[RelationMention]=IndexedSeq.empty,
                        eventMentions: IndexedSeq[EventMention]=IndexedSeq.empty,
                        semanticFrames: IndexedSeq[SemanticFrame]= IndexedSeq.empty) {

  def semanticRoleAt(i: Int, j: Int): Option[SemanticFrame] = {
    semanticFrames.find(f => f.roles.exists(r => r.start == i && r.end == j))
  }

  def semanticFrameSpanning(i: Int, j: Int): Option[SemanticFrame] = {
    semanticFrames.find { f =>
      f.leftMostArg.start == i && f.rightMostArg.end == j
    }
  }
}

/**
 * Companion object for the IEAnnotation class.
 */
object IEAnnotation {
  val empty = IEAnnotation(IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty)
}



/**
 * Class to represent discourse annotation
 * @param relations sequence of DiscourseRelation elements
 */

case class DiscourseAnnotation(relations: Seq[DiscourseRelation] = Seq.empty)


object DiscourseAnnotation {
  val empty = DiscourseAnnotation()
}




/**
 * Class to represent IR information for a document
 * @param docLabel an optional document label.
 * @param bowVector a vectorized bag of word representation, for example using tf-idf counts.
 */
case class IRAnnotation(docLabel:Option[String] = None,
                        bowVector:Option[SparseVector[Double]] = None)

object IRAnnotation {
  val empty = IRAnnotation()
}



/**
 * A container for syntactic annotation.
 * @param tree constituent tree for the sentence.
 * @param dependencies dependency tree for the sentence.
 */
case class SyntaxAnnotation(tree: ConstituentTree, dependencies: DependencyTree) {}

/**
 * Companion object for the SyntaxAnnotation class.
 */
object SyntaxAnnotation {
  val empty = SyntaxAnnotation(tree = ConstituentTree.empty, dependencies = DependencyTree.empty)
}


case class SRLAnnotation(frames: Seq[SemanticFrame]) {}
