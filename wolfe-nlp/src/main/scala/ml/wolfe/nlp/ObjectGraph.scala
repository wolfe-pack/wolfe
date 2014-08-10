package ml.wolfe.nlp

import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
trait ObjectGraph {
  def link1toN[Parent, Child, I <: Iterable[Child]](rel: Any, parent: Parent, children: I): I
  def link1toNOrdered[Parent, Child, I <: Seq[Child]](rel: Any, parent: Parent, children: I): I
  def receive[Child, Parent](rel: Any, child: Child): Parent
  def receiveOrdered[Child, Parent, T](rel: Any, child: Child)(f: (Int, Parent) => T): T
}

class SimpleObjectGraph extends ObjectGraph {

  //todo: hashmaps should use case class object identity, not default equality, for hashing and comparing.
  private val map           = new mutable.HashMap[(Any, Any), Any]()
  private val mapForOrdered = new mutable.HashMap[(Any, Any), (Int, Any)]()


  def link1toNOrdered[Parent, Child, I <: Seq[Child]](rel: Any, parent: Parent, children: I) = {
    for ((c, i) <- children.zipWithIndex) mapForOrdered(c -> rel) = i -> parent
    children

  }
  def link1toN[Parent, Child, I <: Iterable[Child]](rel: Any, parent: Parent, children: I) = {
    for (c <- children) map(c -> rel) = parent
    children
  }
  def receive[Child, Parent](rel: Any, child: Child) =
    map(child -> rel).asInstanceOf[Parent]

  def receiveOrdered[Child, Parent, T](rel: Any, child: Child)(f: (Int, Parent) => T) = {
    val p = mapForOrdered(child -> rel).asInstanceOf[(Int, Parent)]
    f(p._1, p._2)
  }
}
