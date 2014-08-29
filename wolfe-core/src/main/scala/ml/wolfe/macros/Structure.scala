package ml.wolfe.macros

import ml.wolfe.FactorGraph
import ml.wolfe.FactorGraph.Factor

/**
 * A structure is a collection of FactorGraph nodes whose assignments correspond to values of type `T`.
 * @tparam T the type of the values this structure can generate.
 * @author Sebastian Riedel
 */
trait Structure[T] {

  //todo: a lot of functionality in the current implementations can be lifted into this trait
  //todo: by introducting a generic "children:List[Structure]" method and implementing nodes etc. using this.

  /**
   * @return direct child structures of this structure.
   */
  def children(): Iterator[Structure[Any]] //or Structure[_]?

  /**
   * @return all nodes in this structure (including nodes of substructures)
   */
  def nodes(): Iterator[FactorGraph.Node]

  /**
   * @return the underling factor graph that owns the nodes of this structure.
   */
  def graph: FactorGraph

  /**
   * @return the value that the current assignment to all nodes is representing.
   */
  def value(): T

  /**
   * Modifies the assignment to the nodes of the structure to represent the given value.
   * @param value the value the nodes should represent.
   */
  def observe(value: T)

  /**
   * resets the state of all nodes.
   */
  def resetSetting()
  /**
   * @return is there a next state that the structure can take on or have we iterated over all its states.
   */
  def hasNextSetting: Boolean
  /**
   * set the structure to its next state by changing one or more of its nodes assignments.
   */
  def nextSetting()

  /* what this structure represents in the original AST */
  val astLabel:String

  /**
   * Each type of structure has a way to represent itself through a set of factor graph edges.
   */
  type Edges

  /**
   * Create a set of edges from the factor to the nodes that represent this structure.
   * @param factor the factor to connect the nodes to.
   * @return a representation of the edges that were created. This representation depends on the
   *         type of structure. For example, for a sequence structure the edge representation
   *         is a sequence of edge representations of the element structure.
   */
  def createEdges(factor: Factor): Edges


}

object Structure {
  def loopSettings(structures: List[Structure[_]])(loop: () => Unit): () => Unit = structures match {
    case Nil => loop
    case head :: tail =>
      def newLoop() {
        head.resetSetting()
        while (head.hasNextSetting) {
          head.nextSetting()
          loop()
        }
      }
      loopSettings(tail)(newLoop)
  }

  def settingsIterator(structures: List[Structure[_]],
                       iterator: () => Iterator[Unit] = () => Iterator.empty): () => Iterator[Unit] = structures match {
    case Nil => iterator
    case head :: tail =>
      def newIterator() = new Iterator[Unit] {
        head.resetSetting()
        var inner = iterator()
        if (inner.hasNext) {
          head.nextSetting() //this may not work if head has empty domain
        }
        override def next() = {
          if (inner.hasNext) inner.next()
          else {
            head.nextSetting()
            inner = iterator()
            if (inner.hasNext) inner.next()
          }
        }
        override def hasNext = inner.hasNext || head.hasNextSetting
      }
      settingsIterator(tail, newIterator)
  }


}

