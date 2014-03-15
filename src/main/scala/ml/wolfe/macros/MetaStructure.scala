package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * Represents code that generates structures for a given sample space.
 *
 * @author Sebastian Riedel
 */
trait MetaStructure extends HasUniverse {

  import universe._

  /**
   * @return The name of the structure class.
   */
  def className: TypeName

  /**
   * @return A list of definitions of domain values needed in the structure generation code.
   */
  def domainDefs: List[ValDef]

  /**
   * Creates the code the defines the structure class.
   * @param graphName the structure class needs an underlying factor graph, and this parameter provides its name.
   *                  This means that when instantiating the structure class a factor graph with the given name needs
   *                  to be in scope.
   * @return code that defines the structure class.
   */
  def classDef(graphName: TermName): ClassDef

  /**
   * @return the type of objects this structure represents (i.e. the type of objects in the sample space).
   */
  def argType: Type

  /**
   * @return any meta structure used within this meta structure.
   */
  def children: List[MetaStructure]

  /**
   * @return this and all descendant metastructures.
   */
  def all: List[MetaStructure] = this :: children.flatMap(_.all)

  /**
   * A matcher takes a tree and returns a tree that represents the sub-structure that corresponds to the
   * tree, if any. This method creates matchers for the structures of this meta-structure.
   * @param parent a matcher for the parent structure.
   * @param result the current result matcher.
   * @return a matcher...
   */
  def matcher(parent: Tree => Option[Tree], result: Tree => Option[Tree]): Tree => Option[Tree]

}

/**
 * For sample spaces that have no sub-structure.
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


object MetaStructure {

  import scala.language.experimental.macros

  /**
   * Creates a meta structure for the given code repository and sample space.
   * @param repo the code repository to use when inlining.
   * @param sampleSpace the sample space to create a meta structure for.
   * @return
   */
  def apply(repo: CodeRepository)(sampleSpace: repo.universe.Tree): MetaStructure {type U = repo.universe.type} = {
    //todo: assert that domain is an iterable
    sampleSpace match {
      case _ => new MetaAtomicStructure {
        type U = repo.universe.type
        val universe: repo.universe.type = repo.universe
        def repository = repo
        def domain = sampleSpace
      }
    }
  }

  /**
   * Useful for unit tests.
   * @param sampleSpace the sample space to inspect.
   * @tparam T type of objects in sample space.
   * @return a structure corresponding to the given sample space.
   */
  def createStructureMacro[T](sampleSpace: Iterable[T]): Structure[T] = macro createStructureMacroImpl[T]

  def createStructureMacroImpl[T: c.WeakTypeTag](c: Context)(sampleSpace: c.Expr[Iterable[T]]): c.Expr[Structure[T]] = {
    import c.universe._
    val repo = CodeRepository.fromContext(c)
    val meta = MetaStructure(repo)(sampleSpace.tree)
    val graphName = newTermName("_graph")
    val cls = meta.classDef(graphName)
    val code = q"""
      val $graphName = new ml.wolfe.MPGraph
      $cls
      val structure = new ${meta.className}
      $graphName.setupNodes()
      structure
    """
    c.Expr[Structure[T]](code)
  }

}