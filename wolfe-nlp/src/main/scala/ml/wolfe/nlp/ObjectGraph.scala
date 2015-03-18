package ml.wolfe.nlp

import scala.collection.mutable

/**
 * An object graph keeps track of all the relation
 * @author Sebastian Riedel
 *         Ingolf Becker
 *
 */

abstract class ObjectGraphRelation



trait ObjectGraph[Parent, Child] {
  /**
   * Links one child to one parent with the specified relation.
   * @param relation The relation between child and parent
   * @param parent The parent object
   * @param child The Iterable of child object
   * @return The Child
   */
  def link1to1[Rel <: ObjectGraphRelation](relation: Rel, parent: Parent, child: Child): Child
  /**
   * Links all children to the parent with the specified relation.
   * @param relation The relation between child and parents
   * @param parent The parent object
   * @param children The Iterable of child objects
   * @return The children
   */
  def link1toN[I <: Iterable[Child], Rel <: ObjectGraphRelation](relation: Rel, parent: Parent, children: I): I
  /**
   * Gets the parent of this child under this relation
   * @param relation The relation between child and parents
   * @param child The child to find the parent of
   * @return The parent
   */
  def receive[ Rel <: ObjectGraphRelation](relation: Rel, child: Child): Parent
}

class SimpleObjectGraph[Parent, Child] extends ObjectGraph[Parent, Child]  {

  private val map = new mutable.HashMap[(Child, ObjectGraphRelation), Parent]()

  def link1to1[Rel <: ObjectGraphRelation](relation: Rel, parent: Parent, child: Child): Child = {
    map(child -> relation) = parent
    child
  }

  def link1toN[I <: Iterable[Child], Rel <: ObjectGraphRelation](relation: Rel, parent: Parent, children: I): I = {
    for (c <- children) map(c -> relation) = parent
    children
  }
  def receive[ Rel <: ObjectGraphRelation](relation: Rel, child: Child): Parent =
    map(child -> relation)

}

