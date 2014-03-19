package ml.wolfe.macros

import ml.wolfe.MPGraph
import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait StructuredFactor[T] {
  def structure: Structure[T]
  def factors: Iterator[MPGraph.Factor]
  def potential(t: T): Double = {
    structure.observe(t)
    factors.map(_.scoreCurrentSetting).sum
  }
  def graph = structure.graph
}

trait MetaStructuredFactors[C <: Context] extends MetaStructures[C] {

  import context.universe._

  trait MetaStructuredFactor {
    def className: TypeName
    def classDef: Tree
  }

  case class MetaAtomicStructuredFactor(potential:Tree, structure: MetaStructure, matcher: Tree => Option[Tree]) {
    lazy val className = newTypeName(context.fresh("AtomicStructuredFactor"))
    lazy val classDef  = q"""
      import ml.wolfe.macros._
      final class $className(val structure:${structure.className}) extends StructuredFactor[${structure.argType}] {
        def factors = Iterator.empty
        def potential(t:${structure.argType}) = 0.0
      }
    """
  }

  def metaStructuredFactor(potential:Tree, structure:MetaStructure, matcher: Tree => Option[Tree]) = {
    MetaAtomicStructuredFactor(potential,structure,matcher)
  }

}

object MetaStructuredFactor {
  def structuredFactor[T](sampleSpace: Iterable[T]): StructuredFactor[T] = ???
}