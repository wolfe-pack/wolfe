package ml.wolfe.macros

import scala.reflect.macros.Context
import scala.reflect.api.Universe

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
    import repo.universe._
    //get symbol for all, unwrap ...
    val symbols = WolfeSymbols(repo.universe)
    println(symbols.all)
    println(symbols.unwrap)
    sampleSpace match {
      case q"$all[..${_}]($unwrap[..${_}]($constructor))($cross(..$sets))"
        if all.symbol == symbols.all && unwrap.symbol == symbols.unwrap =>
        val t: Type = constructor.tpe
        val scope = t.declarations
        val applySymbol = t.member(newTermName("apply")).asMethod
        val args = applySymbol.paramss.head
        new MetaCaseClassStructure {
          type U = repo.universe.type
          val universe: repo.universe.type = repo.universe
          val tpe                          = constructor.tpe
          val fieldStructures              = sets.map(apply(repo)(_))
          val fields                       = args
          def repository = repo
        }
      case _ =>
        repo.inlineOnce(sampleSpace) match {
          case Some(inlined) => apply(repo)(inlined)
          case None =>
            new MetaAtomicStructure {
              type U = repo.universe.type
              val universe: repo.universe.type = repo.universe
              def repository = repo
              def domain = sampleSpace
            }
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

