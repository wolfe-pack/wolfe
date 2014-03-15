package ml.wolfe.macros

/**
 * For sample spaces that have no sub-structure.
 * @author Sebastian Riedel
 */
trait MetaAtomicStructure extends MetaStructure {
  self =>

  import universe._

  def repository: CodeRepository
  def domain: Tree

  lazy val domName                      = newTermName(repository.fresh("atomDom"))
  lazy val indexName                    = newTermName(repository.fresh("atomIndex"))
  lazy val className                    = newTypeName(repository.fresh("AtomicStructure"))
  lazy val TypeRef(_, _, List(argType)) = domain.tpe
  lazy val argTypeName                  = argType.typeSymbol.name.toTypeName
  lazy val domainDefs                   = List(
    q"val $domName = $domain.toArray",
    q"val $indexName = $domName.zipWithIndex.toMap")
  def children = Nil
  def classDef(graphName: TermName) = q"""
      final class $className extends ml.wolfe.macros.Structure[$argType] {
        ..$domainDefs
        val node = $graphName.addNode($domName.length)
        private def updateValue() {node.value = node.domain(node.setting)}
        def value():$argType = $domName(node.value)
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

  def matcher(parent: Tree => Option[Tree], result: Tree => Option[Tree]): Tree => Option[Tree] = result

}
