package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * Created by luke on 26/08/14.
 */
trait MetaAtomicStructuredFactors[C <: Context] {
  this: MetaStructuredFactors[C] =>

  import context.universe._


  def atomic(info: FactorGenerationInfo) = {
    if(info.structure.hasFiniteDomain) info.linear match {
        case true => MetaDiscreteAtomicStructuredFactorLinear(info)
        case false => MetaDiscreteAtomicStructuredFactorTable(info)
        //    case true => MetaDiscreteAtomicStructuredFactorLinear(info.copy(potential = info.transformer(inlineFull(info.potential))))
        //    case false => MetaDiscreteAtomicStructuredFactorTable(info.copy(potential = info.transformer(inlineFull(info.potential))))
      }
    else info.linear match {
      case true => MetaContinuousAtomicStructuredFactorLinear(info)
      case false => MetaContinuousAtomicStructuredFactorTable(info)
      //    case true => MetaDiscreteAtomicStructuredFactorLinear(info.copy(potential = info.transformer(inlineFull(info.potential))))
      //    case false => MetaDiscreteAtomicStructuredFactorTable(info.copy(potential = info.transformer(inlineFull(info.potential))))
    }
  }



  // ------------------------- Discrete -----------------------------------------

  trait MetaDiscreteAtomicStructuredFactor extends MetaStructuredFactor {
    val info: FactorGenerationInfo
    def perSettingArrayName: TermName
    def perSettingArrayInitializer: Tree
    def perSettingValue: Tree
    def addFactorMethod: TermName = newTermName("addFactor")
    def addEdgeMethod: TermName = newTermName("addEdge")
    def createPotential: Tree
    def children = Nil
    override def weightVector = None

    import info._

    lazy val transformedPot  = transformer(inlineFull(potential))
    lazy val className       = newTypeName(context.fresh("DiscreteAtomicStructuredFactor"))
    lazy val transformedPointers = distinctByTrees(structures(transformedPot, matcher))(_.structure)
    lazy val transformedArgs = transformedPointers.map(_.structure) //distinctTrees(structures(transformedPot, matcher).filterNot(_.meta.observed).map(_.structure))
    lazy val nodesPerArg     = transformedArgs.map(a => q"$a.nodes()")
    lazy val nodes           = q"""Iterator(..$nodesPerArg).flatMap(identity)"""
    lazy val injected        = context.resetLocalAttrs(injectStructure(transformedPot, matcher))
    lazy val constructorArgs = q"val structure:${ structure.className }" :: info.constructorArgs

    def inject(term: Tree) = context.resetLocalAttrs(injectStructure(term, matcher))

    lazy val perSetting = q"""
        /*println(vars.map(_.setting).mkString(","))*/
        settings(settingIndex) = vars.map(_.setting)
        $perSettingArrayName(settingIndex) = $perSettingValue
        settingIndex += 1
      """
    lazy val loop       = transformer(loopSettingsNoDuplicates(transformedPointers) { perSetting })

    lazy val classDef = q"""
      final class $className(..$constructorArgs) extends ml.wolfe.macros.StructuredFactor[${ structure.argType }] {
        import ml.wolfe.FactorGraph._
        val nodes:Array[Node] = $nodes.toList.distinct.sorted.toArray
        val vars = nodes.map(_.variable.asDiscrete)
        val dims = vars.map(_.dim)
        val settingsCount = dims.product
        val settings = Array.ofDim[Array[Int]](settingsCount)
        val $perSettingArrayName = $perSettingArrayInitializer
        var settingIndex = 0
        ml.wolfe.util.Util.breakpoint()
        $loop
        val factor = graph.$addFactorMethod(${shortCode(transformedPot)})
        val edges = nodes.view.zipWithIndex.map(p => graph.$addEdgeMethod(factor,p._1,p._2)).toArray
        factor.potential = $createPotential
        def factors = Iterator(factor)
        def arguments = List(..$transformedArgs)
      }
    """
  }
  case class MetaDiscreteAtomicStructuredFactorTable(info: FactorGenerationInfo)
  extends MetaDiscreteAtomicStructuredFactor {

    import info._

    def createPotential = q"new ml.wolfe.fg.TablePotential(edges,ml.wolfe.fg.Table(settings,scores))"
    def perSettingValue = q"$injected"
    def perSettingArrayInitializer = q"Array.ofDim[Double](settingsCount)"
    def perSettingArrayName = newTermName("scores")
  }

  case class MetaDiscreteAtomicStructuredFactorLinear(info: FactorGenerationInfo)
  extends MetaDiscreteAtomicStructuredFactor {

    import info._


    override def addFactorMethod = if (expectations) newTermName("addExpectationFactor") else newTermName("addFactor")

    override def addEdgeMethod = if (expectations) newTermName("addExpectationEdge") else newTermName("addEdge")

    def createPotential = q"new ml.wolfe.fg.LinearPotential(edges,ml.wolfe.fg.Stats(settings,vectors),graph)"

    def perSettingValue = toOptimizedFactorieVector(injected, linearModelInfo.indexTree)
    //    def perSettingValue = inject(toOptimizedFactorieVector(potential, linearModelInfo.indexTree))
    def perSettingArrayInitializer = q"Array.ofDim[ml.wolfe.FactorieVector](settingsCount)"
    def perSettingArrayName = newTermName("vectors")
  }


  // ------------------------- Continuous  -----------------------------------------


  trait MetaContinuousAtomicStructuredFactor extends MetaStructuredFactor {
    val info: FactorGenerationInfo

    def addFactorMethod: TermName = newTermName("addFactor")
    def addEdgeMethod: TermName = newTermName("addEdge")
    def createPotential: Tree
    def children = Nil
    override def weightVector = None

    import info._

    lazy val transformedPot  = transformer(inlineFull(potential))
    lazy val className       = newTypeName(context.fresh("ContinuousAtomicStructuredFactor"))
    lazy val transformedPointers = distinctByTrees(structures(transformedPot, matcher))(_.structure)
    lazy val transformedArgs = transformedPointers.map(_.structure) //distinctTrees(structures(transformedPot, matcher).filterNot(_.meta.observed).map(_.structure))
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
        val factor = graph.$addFactorMethod(${shortCode(transformedPot)})
        val edges = Nil.toArray
        factor.potential = $createPotential
        def factors = Iterator(factor)
        def arguments = List(..$transformedArgs)
      }
    """

    // val edges = nodes.view.zipWithIndex.map(p => graph.$addEdgeMethod(factor,p._1,p._2)).toArray
  }

  case class MetaContinuousAtomicStructuredFactorLinear(info: FactorGenerationInfo)
  extends MetaContinuousAtomicStructuredFactor {
    import info._
    override def addFactorMethod = if (expectations) newTermName("addExpectationFactor") else newTermName("addFactor")
    override def addEdgeMethod = if (expectations) newTermName("addExpectationEdge") else newTermName("addEdge")
    def createPotential = q"new ml.wolfe.fg.Potential {}"
  }

  case class MetaContinuousAtomicStructuredFactorTable(info: FactorGenerationInfo)
  extends MetaContinuousAtomicStructuredFactor {
    import info._
    def createPotential = q"new ml.wolfe.fg.Potential {}"
  }





















  // -----------------------------------------------------------------------

  def shortCode(t:Tree):Tree = t match {
    case q"ml.wolfe.Wolfe.${x:TermName}" => q"${x.toString}"
    case q"ml.wolfe.macros.OptimizedOperators.${x:TermName}" => q"${x.toString}"
    case q"!( $x )" => q""" "!(" + ${shortCode(x)} + ")" """
    case q"$x.||" => q""" ${shortCode(x)} + " || " """
    case q"$x.^" => q"""${shortCode(x)} + " ^ " """
    case q"$x.&&" => q"""${shortCode(x)} + " && " """
    case q"$x.==" => q"""${shortCode(x)} + " == " """
    case q"$x.>" => q"""${shortCode(x)} + " > " """
    case q"$x.<" => q"""${shortCode(x)} + " < " """
    case q"$x.>=" => q"""${shortCode(x)} + " >= " """
    case q"$x.<=" => q"""${shortCode(x)} + " <= " """
    case q"$x.+" => q"""${shortCode(x)} + " + " """
    case q"$x.-" => q"""${shortCode(x)} + " - " """
    case q"$x.*" => q"""${shortCode(x)} + " * " """
    case q"$x./" => q"""${shortCode(x)} + " / " """
    case q"$x.apply" => shortCode(x)
    case q"qSumDom1($x)" => t
    case q"qSumDom2($x)" => t
    case q"qSumDom3($x)" => t
    case q"qSumDom4($x)" => t
    case q"qSumDom5($x)" => t
    case q"qSumDom6($x)" => t
    case q"${x:Select}" =>  q"""${shortCode(x.qualifier)} + "." + ${x.name.toString} """
    case q"${f:Select}($arg)" => q"""${shortCode(f)} + "(" + ${shortCode(arg)} + ")" """
    case _ => q"${t.toString}"
  }
}
