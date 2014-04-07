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
    def classDef(graphName: TermName) = q"""
      final class $className extends ml.wolfe.macros.Structure[$argType] {
        ..$domainDefs
        val node = $graphName.addNode($domName.length)
        private def updateValue() {node.value = node.domain(node.setting)}
        def value():$argType = $domName(node.value)
        def children():Iterator[ml.wolfe.macros.Structure[_]] = Iterator.empty
        def graph = $graphName
        def nodes() = Iterator(node)
        def resetSetting() {node.setting = -1}
        def hasNextSetting = node.setting < node.dim - 1
        def nextSetting() = {node.setting += 1; updateValue()}
        def setToArgmax() { node.setting = ml.wolfe.MoreArrayOps.maxIndex(node.b); updateValue()}
        final def observe(value:$argType) {
          val index = $indexName(value)
          node.domain = Array(index)
          node.dim = 1
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

    lazy val className = newTypeName(context.fresh("AtomicObservedStructure"))
    lazy val argType   = iterableArgumentType(domain)
    lazy val domainDefs  = Nil
    def children = Nil
    def classDef(graphName: TermName) = q"""
      final class $className extends ml.wolfe.macros.Structure[$argType] {
        private var _value:$argType = _
        val node = $graphName.addNode(1)
        def value():$argType = _value
        def children():Iterator[ml.wolfe.macros.Structure[_]] = Iterator.empty
        def graph = $graphName
        def nodes() = Iterator(node)
        def resetSetting() {node.setting = -1}
        def hasNextSetting = node.setting < node.dim - 1
        def nextSetting() = {node.setting += 1}
        def setToArgmax() { node.setting = 0}
        final def observe(value:$argType) {
          _value = value
        }
      }
    """

    def matcher(parent: Tree => Option[StructurePointer],
                result: Tree => Option[StructurePointer]): Tree => Option[StructurePointer] = result

  }

}
