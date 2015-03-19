package ml.wolfe.nlp

import scala.collection.mutable

/**
 * An object graph keeps track of all the relation
 * @author Sebastian Riedel
 *         Ingolf Becker
 *
 */
trait ObjectGraphRelation {
  type Parent
  type Child
  abstract class RelationType
}


trait ObjectGraph[types <: ObjectGraphRelation] {
  /**
   * Links one child to one parent with the specified relation.
   * @param parent The parent object
   * @param child The Iterable of child object
   * @return The Child
   */
  def link1to1(parent: types#Parent, child: types#Child): types#Child
  /**
   * Links all children to the parent with the specified relation.
   * @param parent The parent object
   * @param children The Iterable of child objects
   * @return The children
   */
  def link1toN[I <: Iterable[types#Child]](parent: types#Parent, children: I): I
  /**
   * Gets the parent of this child under this relation
   * @param child The child to find the parent of
   * @return The parent
   */
  def receive(child: types#Child): types#Parent
}

class SimpleObjectGraph[types <: ObjectGraphRelation] extends ObjectGraph[types] {
  private val map = new mutable.HashMap[types#Child, types#Parent]()

  def link1to1(parent: types#Parent, child: types#Child): types#Child = {
    map(child) = parent
    child
  }

  def link1toN[I <: Iterable[types#Child]](parent: types#Parent, children: I): I = {
    for (c <- children) map(c) = parent
    children
  }

  def receive(child: types#Child): types#Parent = map(child)
}

