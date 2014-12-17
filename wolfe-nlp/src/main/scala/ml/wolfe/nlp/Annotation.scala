package ml.wolfe.nlp

import scala.collection.mutable.ArrayBuffer
import ml.wolfe.nlp.io._

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

}

/**
 * Companion object for the IEAnnotation class.
 */
object IEAnnotation {
  val empty = IEAnnotation(IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty)
}

/**
 * A mention of a named entity.
 * @param label label of the entity.
 * @param start index to the token that begins the entity span.
 * @param end index to the token that ends the entity span.
 * @param id mention-specific identifier of the entity span.
 */
case class EntityMention(label: String, start: Int, end: Int, id: String = null) {
  def expandRight(howMuch:Int = 1) = copy(end = end + howMuch)
}

/**
 * A directed relation mention.
 * @param label label of the relation.
 * @param arg1 index into sentence.entities() for the first argument (parent of the relation)
 * @param arg2 index into sentence.entities() for the second argument (child of the relation)
 */
case class RelationMention(label: String, arg1: Int, arg2: Int, id: String = null) {}

/**
 * An event mention.
 * @param label label of the event.
 * @param trigger trigger word for the event.
 * @param arguments a sequence of argument roles.
 */
case class EventMention(label: String, trigger: EntityMention, arguments: IndexedSeq[RoleMention], id: String = null) {}

/**
 * A role mention.
 * @param label label of the role.
 * @param arg the target of the role.
 */
case class RoleMention(label: String, arg: EntityMention) {}



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

case class SemanticFrame(predicate: Predicate, roles: Seq[SemanticRole]) {

  override def toString = {
    "Predicate %d (%s):\n%s".format(predicate.idx, predicate.token.word,
      roles.map(r => "  %s --> %d".format(r.role, r.idx)).mkString("\n"))
  }
}

case class Predicate(idx: Int, token: Token, sense: String)

case class SemanticRole(idx: Int, role: String)