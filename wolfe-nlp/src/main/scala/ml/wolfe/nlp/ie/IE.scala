package ml.wolfe.nlp.ie

import ml.wolfe.nlp.semantics.RoleMention

/**
 * Created by narad on 3/20/15.
 */
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

