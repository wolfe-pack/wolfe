package ml.wolfe.macros

import scala.reflect.macros.Context
import ml.wolfe.Wolfe

/**
 * Functionality for creating structured factors.
 *
 * @tparam C type of context.
 * @author Sebastian Riedel
 */
trait MetaStructuredFactors[C <: Context] extends MetaStructures[C] {

  import context.universe._

  trait MetaStructuredFactor {
    def className: TypeName
    def classDef: Tree
  }

  case class MetaSumFactor(potentials: List[Tree],
                           factors: List[MetaStructuredFactor],
                           structure: MetaStructure,
                           args: List[ValDef] = Nil) extends MetaStructuredFactor {
    lazy val className       = newTypeName(context.fresh("SumFactor"))
    lazy val argClasses      = factors.map(_.classDef)
    lazy val fieldNames      = for (i <- factors.indices) yield newTermName("arg" + i)
    lazy val fieldIds        = fieldNames.map(name => q"$name")
    lazy val constructorArgs = q"val structure:${structure.className}" :: args
    lazy val childArgs       = q"structure" :: args.map(a => q"${a.name}")

    lazy val setupChildren = for (i <- factors.indices) yield
      q"val ${fieldNames(i)} = new ${factors(i).className}(..$childArgs)"

    lazy val classDef = q"""
      final class $className(..$constructorArgs) extends ml.wolfe.macros.StructuredFactor[${structure.argType}] {
        ..$argClasses
        ..$setupChildren
        def arguments = ???
        def factors = Iterator(..$fieldIds).flatMap(_.factors)
      }
    """
  }

  //todo: make tail recursive
  def tupleProcessor(domainIds: List[TermName], tmpIds: List[TermName], body: Tree,
                     op: TermName = newTermName("flatMap"), lastOp: TermName = newTermName("map")): Tree =
    (domainIds, tmpIds) match {
      case (dom :: Nil, id :: Nil) => q"Range(0,$dom.length).$lastOp($id => $body)"
      case (dom :: domTail, id :: idTail) =>
        val inner = tupleProcessor(domTail, idTail, body, op)
        q"Range(0,$dom.length).$op($id => $inner)"
      case _ => sys.error("shouldn't happen")
    }


  case class MetaFirstOrderSumFactor(domains: List[Tree], obj: Tree,
                                     matchStructure: Tree => Option[Tree], structure: MetaStructure,
                                     args: List[ValDef] = Nil,
                                     differentiatorInfo: Option[DifferentiatorInfo],
                                     linear: Boolean) extends MetaStructuredFactor {
    val className                  = newTypeName(context.fresh("FirstOrderSumFactor"))
    val q"(..$objArgs) => $objRhs" = obj
    //domains may contain references to values in the sample space.
    val injectedDoms               = domains.map(injectStructure(_, matchStructure))
    val keyDomNames                = List.fill(domains.size)(newTermName(context.fresh("qSumDom")))
    val keyDomSizes                = keyDomNames.map(k => q"$k.length")
    val tmpNames                   = Range(0, domains.size).map(i => newTermName(context.fresh("i" + i))).toList
    val tmpIds                     = tmpNames.map(Ident(_))
    val domainDefs                 = for ((d, n) <- injectedDoms zip keyDomNames) yield q"val $n = $d.toArray"
    val ownParams                  = q"val structure:${structure.className}" :: args
    val childParams                = args ::: tmpNames.map(id => q"val $id:Int")
    val childArgs                  = (q"structure" :: args.map(a => q"${a.name}")) ::: tmpIds
    val substitutedObj             = transform(objRhs, {
      case i: Ident if objArgs.exists(_.symbol == i.symbol) =>
        val index = objArgs.indexWhere(_.symbol == i.symbol)
        val replacement = q"${keyDomNames(index)}(${tmpIds(index)})"
        replacement
    })

    val child         = metaStructuredFactor(substitutedObj, structure, matchStructure, childParams, differentiatorInfo, linear)
    val setupChildren = tupleProcessor(keyDomNames, tmpNames, q"new ${child.className}(..$childArgs)")

    val classDef = q"""
      final class $className(..$ownParams) extends ml.wolfe.macros.StructuredFactor[${structure.argType}] {
        ..$domainDefs
        ${child.classDef}
        val factorArray = $setupChildren.toArray
        def arguments = ???
        def factors = factorArray.iterator.flatMap(_.factors)
      }
    """
  }

  case class DifferentiatorInfo(param: Symbol, indexTree: Tree)

  trait MetaAtomicStructuredFactor extends MetaStructuredFactor {
    def potential: Tree
    def structure: MetaStructure
    def matcher: Tree => Option[Tree]
    def args: List[ValDef]
    def perSettingArrayName: TermName
    def perSettingArrayInitializer: Tree
    def perSettingValue: Tree
    def createFactor: Tree

    lazy val className       = newTypeName(context.fresh("AtomicStructuredFactor"))
    lazy val arguments       = distinctTrees(structures(potential, matcher))
    lazy val nodesPerArg     = arguments.map(a => q"$a.nodes()")
    lazy val nodes           = q"""Iterator(..$nodesPerArg).flatMap(identity)"""
    lazy val injected        = context.resetLocalAttrs(injectStructure(potential, matcher))
    lazy val constructorArgs = q"val structure:${structure.className}" :: args

    lazy val perSetting = q"""
        //println(nodes.map(_.setting).mkString(","))
        settings(settingIndex) = nodes.map(_.setting)
        $perSettingArrayName(settingIndex) = $perSettingValue
        settingIndex += 1
      """
    lazy val loop       = loopSettings(arguments) {perSetting}

    lazy val classDef = q"""
      final class $className(..$constructorArgs) extends ml.wolfe.macros.StructuredFactor[${structure.argType}] {
        import ml.wolfe.MPGraph._
        val nodes:Array[Node] = $nodes.toArray
        val dims = nodes.map(_.dim)
        val settingsCount = dims.product
        val settings = Array.ofDim[Array[Int]](settingsCount)
        val $perSettingArrayName = $perSettingArrayInitializer
        var settingIndex = 0
        $loop
        val factor = $createFactor
        nodes.view.zipWithIndex.foreach(p => graph.addEdge(factor,p._1,p._2))
        def factors = Iterator(factor)
        def arguments = List(..$arguments)
      }
    """
  }
  case class MetaAtomicStructuredFactorTable(potential: Tree,
                                             structure: MetaStructure,
                                             matcher: Tree => Option[Tree], args: List[ValDef] = Nil)
  extends MetaAtomicStructuredFactor {

    def createFactor = q"graph.addTableFactor(scores, settings, dims)"
    def perSettingValue = q"$injected"
    def perSettingArrayInitializer = q"Array.ofDim[Double](settingsCount)"
    def perSettingArrayName = newTermName("scores")
  }

  case class MetaAtomicStructuredFactorLinear(potential: Tree,
                                              structure: MetaStructure,
                                              matcher: Tree => Option[Tree],
                                              diffInfo: DifferentiatorInfo,
                                              args: List[ValDef] = Nil)
  extends MetaAtomicStructuredFactor {

    def createFactor = q"graph.addLinearFactor(vectors, settings, dims)"
    def perSettingValue = q"ml.wolfe.FactorieConverter.toFactorieSparseVector($injected,${diffInfo.indexTree})"
    def perSettingArrayInitializer = q"Array.ofDim[ml.wolfe.FactorieVector](settingsCount)"
    def perSettingArrayName = newTermName("vectors")
  }

  def atomic(potential: Tree, structure: MetaStructure, matcher: Tree => Option[Tree], constructorArgs: List[ValDef],
             differentiatorInfo: Option[DifferentiatorInfo], linear: Boolean) = linear match {
    case true => MetaAtomicStructuredFactorLinear(potential, structure, matcher, differentiatorInfo.get, constructorArgs)
    case false => MetaAtomicStructuredFactorTable(potential, structure, matcher, constructorArgs)
  }

  def metaStructuredFactor(potential: Tree, structure: MetaStructure,
                           matcher: Tree => Option[Tree],
                           constructorArgs: List[ValDef] = Nil,
                           differentiatorInfo: Option[DifferentiatorInfo] = None,
                           linear: Boolean = false): MetaStructuredFactor = {
    potential match {
      case DoubleSum(dom, obj, _) =>
        MetaFirstOrderSumFactor(List(dom), obj, matcher, structure, constructorArgs, differentiatorInfo, linear)
      case Apply(f, args) if f.symbol.annotations.exists(_.tpe.typeSymbol == wolfeSymbols.atomic) =>
        atomic(potential, structure, matcher, constructorArgs, differentiatorInfo, linear)
      case ApplyDoublePlus(arg1, arg2) =>
        val f1 = metaStructuredFactor(arg1, structure, matcher, constructorArgs, differentiatorInfo, linear)
        val f2 = metaStructuredFactor(arg2, structure, matcher, constructorArgs, differentiatorInfo, linear)
        MetaSumFactor(List(arg1, arg2), List(f1, f2), structure, constructorArgs)
      case Dot(arg1, arg2) if differentiatorInfo.exists(i => i.param == arg2.symbol && !arg1.exists(_.symbol == i.param)) =>
        metaStructuredFactor(arg1, structure, matcher, constructorArgs, differentiatorInfo, true)
      case Dot(arg2, arg1) if differentiatorInfo.exists(i => i.param == arg2.symbol && !arg1.exists(_.symbol == i.param)) =>
        metaStructuredFactor(arg1, structure, matcher, constructorArgs, differentiatorInfo, true)
      case _ => inlineOnce(potential) match {
        case Some(inlined) =>
          metaStructuredFactor(inlined, structure, matcher, constructorArgs, differentiatorInfo, linear)
        case None =>
          atomic(potential, structure, matcher, constructorArgs, differentiatorInfo, linear)
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
  def structuredFactor[T](sampleSpace: Iterable[T],
                          potential: T => Double): StructuredFactor[T] =  macro structuredFactorImpl[T]

  def structuredLinearFactor[T](sampleSpace: Iterable[T],
                                potential: Wolfe.Vector => T => Double): Wolfe.Vector => StructuredFactor[T] = macro structuredLinearFactorImpl[T]


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

  def structuredLinearFactorImpl[T: c.WeakTypeTag](c: Context)(sampleSpace: c.Expr[Iterable[T]],
                                                               potential: c.Expr[Wolfe.Vector => T => Double]) = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with MetaStructuredFactors[c.type]

    val graphName = newTermName("_graph")
    val structName = newTermName("structure")
    val meta = helper.metaStructure(sampleSpace.tree)
    val q"($param) => ($arg) => $rhs" = helper.simplifyBlocks(potential.tree)
    val root = helper.rootMatcher(arg.symbol, q"$structName")
    val matcher = meta.matcher(root)
    val diffInfo = helper.DifferentiatorInfo(param.symbol,q"_index")
    val metaFactor = helper.metaStructuredFactor(rhs, meta, matcher,differentiatorInfo = Some(diffInfo))
    val structureClass = meta.classDef(graphName)
    val code = q"""
      (weights:ml.wolfe.Wolfe.Vector) => {
        val $graphName = new ml.wolfe.MPGraph
        val _index = new ml.wolfe.Index()
        $structureClass
        val $structName = new ${meta.className}
        $graphName.setupNodes()
        ${metaFactor.classDef}
        val result:StructuredFactor[${meta.argType}] = new ${metaFactor.className}($structName)
        $graphName.build()
        val factorieWeights = ml.wolfe.FactorieConverter.toFactorieSparseVector(weights,_index)
        $graphName.weights = factorieWeights
        result
      }
    """
    c.Expr[Wolfe.Vector => StructuredFactor[T]](code)
  }


}