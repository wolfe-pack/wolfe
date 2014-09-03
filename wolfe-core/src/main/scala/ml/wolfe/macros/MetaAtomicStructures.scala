package ml.wolfe.macros

import scala.reflect.macros.Context


trait MetaAtomicStructures[C <: Context] {
  this: MetaStructures[C] =>

  import context.universe._

  /**
   * For sample spaces that have no sub-structure.
   * @author Sebastian Riedel
   */
  trait MetaDiscreteAtomicStructure extends MetaStructure {
    self =>

    def domain: Tree

    lazy val domName   = newTermName(context.fresh("atomDom"))
    override lazy val className = newTypeName(context.fresh("DiscreteAtomicStructure"))
    override lazy val argType   = iterableArgumentType(domain)
    //{val TypeRef(_, _, List(argType)) = domain.tpe; argType}
    override lazy val domainDefs  = List(
      q"val $domName = $domain.toArray"
    )
    override def children = Nil

    def edgesType = tq"ml.wolfe.FactorGraph.Edge"
    override def classDef(graphName: TermName) = q"""
      final class $className (override val astLabel : String = "") extends ml.wolfe.macros.Structure[$argType] {
        ..$domainDefs
        val node = $graphName.addDiscreteNodeWithDomain($domName, astLabel)
        val variable = node.variable.asDiscreteTyped[$argType]
        def value():$argType = variable.value
        def children():Iterator[ml.wolfe.macros.Structure[Any]] = Iterator.empty
        def graph = $graphName
        def nodes() = Iterator(node)
        def resetSetting() {variable.setting = -1}
        def hasNextSetting = variable.setting < variable.dim - 1
        def nextSetting() = {variable.setting += 1}
        final def observe(value:$argType) {
          variable.domain = Array(value)
        }
        type Edges = ml.wolfe.FactorGraph.Edge
        def createEdges(factor: ml.wolfe.FactorGraph.Factor): Edges = {
          graph.addEdge(factor,node)
        }

      }
    """

    def matcher(parent: Tree => Option[StructurePointer],
                result: Tree => Option[StructurePointer]): Tree => Option[StructurePointer] = result

    override def hasFiniteDomain: Boolean = true
  }

  /**
   * For sample spaces that will be fully observed.
   * @author Sebastian Riedel
   */
  trait MetaObservedAtomicStructure extends MetaStructure {
    self =>

    def domain: Tree

    override lazy val className = newTypeName(context.fresh("AtomicObservedStructure"))
    override lazy val argType   = iterableArgumentType(domain)
    override lazy val domainDefs  = Nil
    override def children = Nil

    def edgesType = tq"Unit"
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
        
        final def observe(value:$argType) {
          _value = value
        }
        type Edges = Unit
        def createEdges(factor: ml.wolfe.FactorGraph.Factor): Edges = {}

      }
    """

    def matcher(parent: Tree => Option[StructurePointer],
                result: Tree => Option[StructurePointer]): Tree => Option[StructurePointer] = result

    override def hasFiniteDomain: Boolean = observed
  }




  /**
   * For sample spaces that have no sub-structure.
   * @author Sebastian Riedel
   */
  trait MetaContinuousAtomicStructure extends MetaStructure {
    self =>

    def domain: Tree

    override lazy val className = newTypeName(context.fresh("ContinuousAtomicStructure"))
    override lazy val argType   = iterableArgumentType(domain)
    //{val TypeRef(_, _, List(argType)) = domain.tpe; argType}
    override lazy val domainDefs  = List()
    def children = Nil

    def edgesType = tq"ml.wolfe.FactorGraph.Edge"
    override def classDef(graphName: TermName) = q"""
      final class $className (override val astLabel: String = "") extends ml.wolfe.macros.Structure[$argType] {
        val node = $graphName.addContinuousNode(astLabel)
        val variable = node.variable.asContinuous
        def value():Double = node.variable.asTyped[Double].value
        def children():Iterator[ml.wolfe.macros.Structure[Any]] = Iterator.empty
        def graph = $graphName
        def nodes() = Iterator(node)

        private var _hasNext = true
        def resetSetting() = if(variable.isObserved) _hasNext = true else ???
        def hasNextSetting = if(variable.isObserved) _hasNext else ???
        def nextSetting() = if(variable.isObserved) _hasNext = false else ???

        final def observe(value:$argType) {
          variable.observe(value)
        }
        type Edges = ml.wolfe.FactorGraph.Edge
        def createEdges(factor: ml.wolfe.FactorGraph.Factor): Edges = {
          graph.addEdge(factor,node)
        }

      }
    """

    def matcher(parent: Tree => Option[StructurePointer],
                result: Tree => Option[StructurePointer]): Tree => Option[StructurePointer] = result

    override def hasFiniteDomain: Boolean = observed
  }

}
