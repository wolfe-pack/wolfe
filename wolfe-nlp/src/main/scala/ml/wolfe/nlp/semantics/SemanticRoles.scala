package ml.wolfe.nlp.semantics

import ml.wolfe.nlp.Token
import ml.wolfe.nlp.ie.EntityMention
import ml.wolfe.nlp.syntax.{DependencyTree, ConstituentTree}

/**
 * Created by narad on 3/20/15.
 */
/**
 * A role mention.
 * @param label label of the role.
 * @param arg the target of the role.
 */
case class RoleMention(label: String, arg: EntityMention) {}


case class SemanticFrame(predicate: Predicate, roles: Seq[SemanticRole]) {

  def leftMostArg = roles.sortBy(_.start).head

  def rightMostArg = roles.sortBy(_.end).last

  override def toString = {
    "Predicate %d (%s):\n%s".format(predicate.idx, predicate.token.word,
      roles.map{ r =>
        if (r.end == -1) "  %s --> %d".format(r.role, r.idx)
        else "  %s --> (%d,%d)".format(r.role, r.start, r.end)
      }.mkString("\n"))
  }
}

case class Predicate(idx: Int, token: Token, sense: String)

case class SemanticRole(start: Int, end: Int = -1, role: String) {

  def idx: Int = {
    assert(end == -1, "This SemanticRole is a span-based instance, and therefore indices should be referenced using start/end fields.")
    start
  }
}
