package ml.wolfe.macros

import ml.wolfe.MPGraph
import scala.reflect.macros.Context

/**
 * Represents a factor that decomposes into sub-factors, and provides a potential over
 * values of type `T`.
 * @author Sebastian Riedel
 */
trait StructuredFactor[T] {

  /**
   * @return the top level structure corresponding to all nodes of the factor graph.
   */
  def structure: Structure[T]

  /**
   * @return the sub-structures involved in this factor.
   */
  def arguments: List[Structure[_]]

  /**
   * @return the factor graph factors this structured factor corresponds to.
   */
  def factors: Iterator[MPGraph.Factor]

  /**
   * @return the score of the structures current setting under the factor's potential.
   */
  def potential(): Double = {
    factors.map(_.scoreCurrentSetting).sum
  }

  /**
   * @return the underlying factor graph.
   */
  def graph = structure.graph
}

/**
 * Functionality for creating structured factors.
 *
 * @tparam C type of context.
 */
trait MetaStructuredFactors[C <: Context] extends MetaStructures[C] {

  import context.universe._

  trait MetaStructuredFactor {
    def className: TypeName
    def classDef: Tree
  }

  case class MetaSumFactor(potentials: List[Tree], factors: List[MetaStructuredFactor], structure: MetaStructure) extends MetaStructuredFactor{
    lazy val className  = newTypeName(context.fresh("SumFactor"))
    lazy val argClasses = factors.map(_.classDef)
    lazy val fieldNames = for (i <- factors.indices) yield newTermName("arg" + i)
    lazy val fieldIds   = fieldNames.map(name => q"$name")

    lazy val setupChildren = for (i <- factors.indices) yield
      q"val ${fieldNames(i)} = new ${factors(i).className}(structure)"

    lazy val classDef = q"""
      final class $className(val structure:${structure.className}) extends ml.wolfe.macros.StructuredFactor[${structure.argType}] {
        ..$argClasses
        ..$setupChildren
        def arguments = ???
        def factors = Iterator(..$fieldIds).flatMap(_.factors)
      }
    """

  }

  case class MetaAtomicStructuredFactor(potential: Tree,
                                        structure: MetaStructure,
                                        matcher: Tree => Option[Tree]) extends MetaStructuredFactor {
    lazy val className   = newTypeName(context.fresh("AtomicStructuredFactor"))
    lazy val arguments   = distinctTrees(structures(potential, matcher))
    lazy val nodesPerArg = arguments.map(a => q"$a.nodes()")
    lazy val nodes       = q"""Iterator(..$nodesPerArg).flatMap(identity)"""
    lazy val injected    = context.resetLocalAttrs(injectStructure(potential, matcher))

    lazy val classDef = q"""
      final class $className(val structure:${structure.className}) extends ml.wolfe.macros.StructuredFactor[${structure.argType}] {
        import ml.wolfe.MPGraph._
        val nodes:Array[Node] = $nodes.toArray
        val dims = nodes.map(_.dim)
        val settingsCount = dims.product
        val settings = Array.ofDim[Array[Int]](settingsCount)
        val scores = Array.ofDim[Double](settingsCount)
        var settingIndex = 0
        $loop
        val factor = graph.addTableFactor(scores, settings, dims)
        nodes.view.zipWithIndex.foreach(p => graph.addEdge(factor,p._1,p._2))
        def factors = Iterator(factor)
        def arguments = List(..$arguments)
      }
    """


    lazy val perSetting = q"""
        //println(nodes.map(_.setting).mkString(","))
        settings(settingIndex) = nodes.map(_.setting)
        scores(settingIndex) = $injected
        settingIndex += 1
      """

    lazy val loop = loopSettings(arguments) {perSetting}


  }

  def metaStructuredFactor(potential: Tree, structure: MetaStructure, matcher: Tree => Option[Tree]): MetaStructuredFactor = {
    println(scalaSymbols.doublePlus)
    potential match {
//      case Apply(f,args) if f.symbol.annotations.exists(_.tpe)
      case ApplyPlus(arg1,arg2) =>
        val f1 = metaStructuredFactor(arg1, structure, matcher)
        val f2 = metaStructuredFactor(arg2, structure, matcher)
        MetaSumFactor(List(arg1,arg2),List(f1,f2),structure)
      case _ => inlineOnce(potential) match {
        case Some(inlined) => metaStructuredFactor(inlined, structure, matcher)
        case None => MetaAtomicStructuredFactor(potential, structure, matcher)
      }
    }
  }

}

object MetaStructuredFactor {

  import scala.language.experimental.macros

  /**
   * @param sampleSpace sample space for factor graph.
   * @param potential potential function.
   * @tparam T type of values in sample space.
   * @return a structured factor isomorphic to `potential`.
   */
  def structuredFactor[T](sampleSpace: Iterable[T], potential: T => Double): StructuredFactor[T] = macro structuredFactorImpl[T]

  def structuredFactorImpl[T: c.WeakTypeTag](c: Context)(sampleSpace: c.Expr[Iterable[T]],
                                                         potential: c.Expr[T => Double]) = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with MetaStructuredFactors[c.type]

    val graphName = newTermName("_graph")
    val structName = newTermName("structure")
    val meta = helper.metaStructure(sampleSpace.tree)
    val q"($arg) => $rhs" = helper.simplifyBlocks(potential.tree)
    val root = helper.rootMatcher(arg.symbol, q"$structName")
    val matcher = meta.matcher(root)
    val metaFactor = helper.metaStructuredFactor(rhs, meta, matcher)
    val structureClass = meta.classDef(graphName)
    val code = q"""
      val $graphName = new ml.wolfe.MPGraph
      $structureClass
      val $structName = new ${meta.className}
      $graphName.setupNodes()
      ${metaFactor.classDef}
      val result:StructuredFactor[${meta.argType}] = new ${metaFactor.className}($structName)
      $graphName.build()
      result
    """
    c.Expr[StructuredFactor[T]](code)
  }

}