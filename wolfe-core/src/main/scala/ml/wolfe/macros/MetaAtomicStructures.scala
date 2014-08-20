package ml.wolfe.macros

import scala.reflect.macros.Context


trait MetaAtomicStructures[C <: Context] {
  this: MetaStructures[C] =>

  import context.universe._

  /**
   * For sample spaces that have no sub-structure.
   * @author Sebastian Riedel
   */
  trait MetaAtomicStructure extends MetaStructure {
    self =>

    def domain: Tree

    lazy val domName   = newTermName(context.fresh("atomDom"))
    lazy val indexName = newTermName(context.fresh("atomIndex"))
    lazy val className = newTypeName(context.fresh("AtomicStructure"))
    lazy val argType   = iterableArgumentType(domain)
    //{val TypeRef(_, _, List(argType)) = domain.tpe; argType}
    lazy val domainDefs  = List(
      q"val $domName = $domain.toArray",
      q"val $indexName = $domName.zipWithIndex.toMap")
    def children = Nil
    override def classDef(graphName: TermName) = q"""
      final class $className (override val astLabel : String = "") extends ml.wolfe.macros.Structure[$argType] {
        ..$domainDefs
        val node = $graphName.addNode($domName.length, astLabel, $domName.map(_.toString))
        val variable = node.variable.asDiscrete
        private def updateValue() {variable.value = variable.domain(variable.setting)}
        def value():$argType = $domName(variable.value)
        def children():Iterator[ml.wolfe.macros.Structure[Any]] = Iterator.empty
        def graph = $graphName
        def nodes() = Iterator(node)
        def resetSetting() {variable.setting = -1}
        def hasNextSetting = variable.setting < variable.dim - 1
        def nextSetting() = {variable.setting += 1; updateValue()}
        def setToArgmax() { /*(Moved to BeliefPropagation) variable.setting = ml.wolfe.MoreArrayOps.maxIndex(variable.b);*/
                            updateValue()
                          }
        final def observe(value:$argType) {
          val index = $indexName(value)
          variable.domain = Array(index)
          variable.dim = 1
        }
        type Edges = ml.wolfe.FactorGraph.Edge
        def createEdges(factor: ml.wolfe.FactorGraph.Factor): Edges = {
          graph.addEdge(factor,node)
        }

      }
    """

    def matcher(parent: Tree => Option[StructurePointer],
                result: Tree => Option[StructurePointer]): Tree => Option[StructurePointer] = result

  }

  /**
   * For sample spaces that will be fully observed.
   * @author Sebastian Riedel
   */
  trait MetaObservedAtomicStructure extends MetaStructure {
    self =>

    def domain: Tree

    override def observed = true
    lazy val className = newTypeName(context.fresh("AtomicObservedStructure"))
    lazy val argType   = iterableArgumentType(domain)
    lazy val domainDefs  = Nil
    def children = Nil
    override def classDef(graphName: TermName) = q"""
      final class $className (override val astLabel : String = "") extends ml.wolfe.macros.Structure[$argType] {
        private var _value:$argType = _
        private var _hasNext = true
        def value():$argType = _value
        def children():Iterator[ml.wolfe.macros.Structure[Any]] = Iterator.empty
        def graph = $graphName
        def nodes() = Iterator.empty
        def resetSetting() {_hasNext = true}
        def hasNextSetting = _hasNext
        def nextSetting() = {_hasNext = false}
        def setToArgmax() {}
        final def observe(value:$argType) {
          _value = value
        }
        type Edges = Unit
        def createEdges(factor: ml.wolfe.FactorGraph.Factor): Edges = {}

      }
    """

    def matcher(parent: Tree => Option[StructurePointer],
                result: Tree => Option[StructurePointer]): Tree => Option[StructurePointer] = result

  }

}
