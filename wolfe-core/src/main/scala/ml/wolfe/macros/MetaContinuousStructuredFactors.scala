package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * Created by luke on 28/08/14.
 */
trait MetaContinuousStructuredFactors[C <: Context] {
  this: MetaStructuredFactors[C] =>

  import context.universe._

  final class MetaGaussianStructuredFactor(info: FactorGenerationInfo, val mean:Tree, val dev:Tree, val x:Tree) extends MetaStructuredFactor {
    def addFactorMethod: TermName = newTermName("addFactor")
    def addEdgeMethod: TermName = newTermName("addEdge")
    def createPotential: Tree = q"""
      new ml.wolfe.fg.GaussianPotential(edges(0), edges(1), edges(2))
      """
    def children = Nil
    override def weightVector = None

    import info._

    lazy val transformedPot  = transformer(inlineFull(potential))
    lazy val className       = newTypeName(context.fresh("GaussianStructuredFactor"))
    //lazy val transformedPointers = distinctByTrees(structures(transformedPot, matcher).filterNot(_.meta.observed))(_.structure)
    //lazy val transformedArgs = transformedPointers.map(_.structure) //distinctTrees(structures(transformedPot, matcher).filterNot(_.meta.observed).map(_.structure))
    lazy val transformedArgs = Seq(mean, dev, x)
    lazy val nodesPerArg     = transformedArgs.map(a => q"$a.nodes()")
    lazy val nodes           = q"""Iterator(..$nodesPerArg).flatMap(identity)"""
    lazy val injected        = context.resetLocalAttrs(injectStructure(transformedPot, matcher))
    lazy val constructorArgs = q"val structure:${ structure.className }" :: info.constructorArgs

    def inject(term: Tree) = context.resetLocalAttrs(injectStructure(term, matcher))

    lazy val classDef = q"""
      final class $className(..$constructorArgs) extends ml.wolfe.macros.StructuredFactor[${ structure.argType }] {
        import ml.wolfe.FactorGraph._
        val nodes:Array[Node] = $nodes.toList.distinct.sorted.toArray
        val vars = nodes.map(_.variable)
        val factor = graph.$addFactorMethod(${MetaStructuredFactor.shortCode(context)(transformedPot)})
        ml.wolfe.util.Util.breakpoint()
        val edges:Array[Edge] = nodes.view.zipWithIndex.map(p => graph.$addEdgeMethod(factor,p._1,p._2)).toArray
        ml.wolfe.util.Util.breakpoint()
        factor.potential = $createPotential
        def factors = Iterator(factor)
        def arguments = List(..$transformedArgs)
      }
    """
  }
}