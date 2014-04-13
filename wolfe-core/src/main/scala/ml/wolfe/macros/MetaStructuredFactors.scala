package ml.wolfe.macros

import scala.reflect.macros.Context
import ml.wolfe.Wolfe
import ml.wolfe.util.CachedPartialFunction

/**
 * Functionality for creating structured factors.
 *
 * @tparam C type of context.
 * @author Sebastian Riedel
 */
trait MetaStructuredFactors[C <: Context] extends MetaStructures[C] with CodeOptimizer[C] {

  import context.universe._

  trait MetaStructuredFactor {
    def className: TypeName
    def classDef: Tree
    def weightVector: Option[Tree] = {
      val childVectors = children.map(_.weightVector).collect({ case Some(w) => w })
      val allEqual = childVectors.forall(w => childVectors.forall(_.equalsStructure(w)))
      if (!allEqual) context.error(context.enclosingPosition, "Different weight vectors appear in model: " + childVectors)
      childVectors.headOption
    }
    def children: List[MetaStructuredFactor]
  }

  case class WithWeightVector(self: MetaStructuredFactor, weight: Tree) extends MetaStructuredFactor {
    def className = self.className
    def classDef = self.classDef
    override def weightVector = Some(weight)
    def children = self.children
  }

  case class MetaSumFactor(potentials: List[Tree],
                           factors: List[MetaStructuredFactor],
                           structure: MetaStructure,
                           args: List[ValDef] = Nil) extends MetaStructuredFactor {
    lazy val className       = newTypeName(context.fresh("SumFactor"))
    lazy val argClasses      = factors.map(_.classDef)
    lazy val fieldNames      = for (i <- factors.indices) yield newTermName("arg" + i)
    lazy val fieldIds        = fieldNames.map(name => q"$name")
    lazy val constructorArgs = q"val structure:${ structure.className }" :: args
    lazy val childArgs       = q"structure" :: args.map(a => q"${ a.name }")

    lazy val setupChildren = for (i <- factors.indices) yield
      q"val ${ fieldNames(i) } = new ${ factors(i).className }(..$childArgs)"

    lazy val classDef = q"""
      final class $className(..$constructorArgs) extends ml.wolfe.macros.StructuredFactor[${ structure.argType }] {
        ..$argClasses
        ..$setupChildren
        def arguments = ???
        def factors = Iterator(..$fieldIds).flatMap(_.factors)
      }
    """
    def children = factors
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
                                     matchStructure: Tree => Option[StructurePointer], structure: MetaStructure,
                                     args: List[ValDef] = Nil,
                                     linearModelInfo: LinearModelInfo,
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
    val ownParams                  = q"val structure:${ structure.className }" :: args
    val childParams                = args ::: tmpNames.map(id => q"val $id:Int")
    val childArgs                  = (q"structure" :: args.map(a => q"${ a.name }")) ::: tmpIds
    val substitutedObj             = transform(objRhs, {
      case i: Ident if objArgs.exists(_.symbol == i.symbol) =>
        val index = objArgs.indexWhere(_.symbol == i.symbol)
        val replacement = q"${ keyDomNames(index) }(${ tmpIds(index) })"
        replacement
    })

    val child         = metaStructuredFactor(substitutedObj, structure, matchStructure, childParams, linearModelInfo, linear)
    val setupChildren = tupleProcessor(keyDomNames, tmpNames, q"new ${ child.className }(..$childArgs)")

    val classDef = q"""
      final class $className(..$ownParams) extends ml.wolfe.macros.StructuredFactor[${ structure.argType }] {
        ..$domainDefs
        ${ child.classDef }
        val factorArray = $setupChildren.toArray
        def arguments = ???
        def factors = factorArray.iterator.flatMap(_.factors)
      }
    """
    def children = List(child)
  }

  case class LinearModelInfo(indexTree: Tree)

  trait MetaAtomicStructuredFactor extends MetaStructuredFactor {
    def potential: Tree
    def structure: MetaStructure
    def matcher: Tree => Option[StructurePointer]
    def args: List[ValDef]
    def perSettingArrayName: TermName
    def perSettingArrayInitializer: Tree
    def perSettingValue: Tree
    def createFactor: Tree
    def children = Nil
    override def weightVector = None

    lazy val className       = newTypeName(context.fresh("AtomicStructuredFactor"))
    lazy val arguments       = distinctTrees(structures(potential, matcher).filterNot(_.meta.observed).map(_.structure))
    //    lazy val arguments       = distinctTrees(structures(potential, matcher).map(_.structure))
    lazy val nodesPerArg     = arguments.map(a => q"$a.nodes()")
    lazy val nodes           = q"""Iterator(..$nodesPerArg).flatMap(identity)"""
    lazy val injected        = context.resetLocalAttrs(injectStructure(potential, matcher))
    lazy val constructorArgs = q"val structure:${ structure.className }" :: args

    lazy val perSetting = q"""
//        println(nodes.map(_.setting).mkString(","))
        settings(settingIndex) = nodes.map(_.setting)
        $perSettingArrayName(settingIndex) = $perSettingValue
        settingIndex += 1
      """
    lazy val loop       = loopSettings(arguments) { perSetting }

    lazy val classDef = q"""
      final class $className(..$constructorArgs) extends ml.wolfe.macros.StructuredFactor[${ structure.argType }] {
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
                                             matcher: Tree => Option[StructurePointer], args: List[ValDef] = Nil)
  extends MetaAtomicStructuredFactor {

    def createFactor = q"graph.addTableFactor(scores, settings, dims)"
    def perSettingValue = q"$injected"
    def perSettingArrayInitializer = q"Array.ofDim[Double](settingsCount)"
    def perSettingArrayName = newTermName("scores")
  }

  case class MetaAtomicStructuredFactorLinear(potential: Tree,
                                              structure: MetaStructure,
                                              matcher: Tree => Option[StructurePointer],
                                              diffInfo: LinearModelInfo,
                                              args: List[ValDef] = Nil)
  extends MetaAtomicStructuredFactor {

    def createFactor = q"graph.addLinearFactor(vectors, settings, dims)"
    def perSettingValue = toOptimizedFactorieVector(injected, diffInfo.indexTree)
    //q"${ diffInfo.indexTree }.toCachedFactorieSparseVector($injected,true)"
    //    def perSettingValue = q"${ diffInfo.indexTree }.toCachedFactorieSparseVector($injected,true)"
    def perSettingArrayInitializer = q"Array.ofDim[ml.wolfe.FactorieVector](settingsCount)"
    def perSettingArrayName = newTermName("vectors")
  }

  def atomic(potential: Tree, structure: MetaStructure, matcher: Tree => Option[StructurePointer], constructorArgs: List[ValDef],
             linearModelInfo: LinearModelInfo, linear: Boolean) = linear match {
    case true => MetaAtomicStructuredFactorLinear(inlineFull(potential), structure, matcher, linearModelInfo, constructorArgs)
    case false => MetaAtomicStructuredFactorTable(inlineFull(potential), structure, matcher, constructorArgs)
  }


  def variablesContainArgument(obj: Tree, matcher: Tree => Option[StructurePointer]) = {
    val Function(List(arg), rhs) = normalize(obj)
    val structs = structures(rhs, matcher)
    val variables = distinctTrees(structs.filterNot(_.meta.observed).map(_.structure))
    variables.exists(_.exists(_.symbol == arg.symbol))
  }


  //merge the arguments of a propositional sum
  def mergeSumArgs(args: List[Tree], matcher: Tree => Option[StructurePointer]) = {

    def mergeTwoArgs(arg1: Tree)(arg2: Tree): Option[(Tree, Tree)] = {
      (arg1, arg2) match {
        case (Sum(BuilderTrees(dom1, filter1, obj1, _, impArg)), Sum(BuilderTrees(dom2, filter2, obj2, _, _)))
          if dom1.equalsStructure(dom2) && filter1 == EmptyTree && filter2 == EmptyTree =>
          val Function(List(p1), rhs1) = normalize(obj1)
          val Function(List(p2), rhs2) = normalize(obj2)
          val rhs2WithP1 = transform(rhs2, {
            case x: Ident if x.symbol == p2.symbol => Ident(p1.symbol)
          })
          val add = q"$rhs1 + $rhs2WithP1"
          val addObj = Function(List(p1), add)
          val newSum = q"$dom1.map($addObj).sum($impArg)"
          val typed = context.typeCheck(context.resetLocalAttrs(newSum))
          Some(arg2 -> typed)
        case (p1,p2) =>
          //check whether p1 and p2 have the same hidden variables. In this case add them into an atomic call
          val structs1 = structures(p1, matcher)
          val structs2 = structures(p2, matcher)
          val vars1 = distinctTrees(structs1.filterNot(_.meta.observed).map(_.structure))
          val vars2 = distinctTrees(structs2.filterNot(_.meta.observed).map(_.structure))
          val mergable = vars1.size == vars2.size && vars1.forall(v1 => vars2.exists(v2 => v1.equalsStructure(v2)))
          if (mergable) {
            val merged = q"ml.wolfe.Wolfe.atomic($p1 + $p2)"
            val typed = context.typeCheck(context.resetLocalAttrs(merged))
            Some(arg2 -> typed)
          } else None
        case _ => None
      }

    }

    val mergeResult = args.foldLeft(List.empty[Tree]) {
      case (result, arg) =>
        result.collectFirst(CachedPartialFunction(mergeTwoArgs(arg))) match {
          case Some((orig, merged)) => result.map(a => if (a == orig) merged else a)
          case None => arg :: result
        }
    }
    mergeResult
  }


  def metaStructuredFactor(potential: Tree, structure: MetaStructure,
                           matcher: Tree => Option[StructurePointer],
                           constructorArgs: List[ValDef] = Nil,
                           linearModelInfo: LinearModelInfo,
                           linear: Boolean = false): MetaStructuredFactor = {
    //    val simplified = unwrapSingletonBlocks(potential)
    val simplified = simplifyBlock(unwrapSingletonBlocks(potential))
    simplified match {
      case Sum(BuilderTrees(dom, filter, obj, _, _)) =>
        require(filter == EmptyTree)
        //check whether we need to further factorize (only if the objective argument is part of the variables)
        if (!variablesContainArgument(obj, matcher))
          atomic(potential, structure, matcher, constructorArgs, linearModelInfo, linear)
        else
          MetaFirstOrderSumFactor(List(dom), obj, matcher, structure, constructorArgs, linearModelInfo, linear)
      case Apply(f, args) if f.symbol.annotations.exists(_.tpe.typeSymbol == wolfeSymbols.atomic) =>
        atomic(potential, structure, matcher, constructorArgs, linearModelInfo, linear)
      case FlattenedPlus(args) =>
        val metaStructs = args.map(arg => metaStructuredFactor(arg, structure, matcher, constructorArgs, linearModelInfo, linear))
        MetaSumFactor(args, metaStructs, structure, constructorArgs)
      case Dot(arg1, arg2) if structures(arg1, matcher).isEmpty =>
        val linearFactor = metaStructuredFactor(arg2, structure, matcher, constructorArgs, linearModelInfo, true)
        WithWeightVector(linearFactor, arg1)
      case Dot(arg2, arg1) if structures(arg1, matcher).isEmpty =>
        val linearFactor = metaStructuredFactor(arg2, structure, matcher, constructorArgs, linearModelInfo, true)
        WithWeightVector(linearFactor, arg1)
      case _ => inlineOnce(potential) match {
        case Some(inlined) =>
          metaStructuredFactor(inlined, structure, matcher, constructorArgs, linearModelInfo, linear)
        case None =>
          atomic(potential, structure, matcher, constructorArgs, linearModelInfo, linear)
      }
    }
  }

  def structuredLinearFactorCode(sampleSpace: Tree, arg: Tree, rhs: Tree) = {
    val structName = newTermName("structure")
    val graphName = newTermName("_graph")
    val meta = metaStructure(sampleSpace)
    val root = rootMatcher(arg.symbol, q"$structName", meta)
    val matcher = meta.matcher(root)
    val metaFactor = metaStructuredFactor(rhs, meta, matcher, linearModelInfo = LinearModelInfo(q"_index"))
    val factorieWeights = metaFactor.weightVector.map(
      w => q"ml.wolfe.FactorieConverter.toFactorieDenseVector($w,_index)"
    ).getOrElse(q"new ml.wolfe.DenseVector(0)")
    val structureClass = meta.classDef(graphName)
    val code = q"""
      val _index = new ml.wolfe.DefaultIndex
      val $graphName = new ml.wolfe.MPGraph
      $structureClass
      val $structName = new ${ meta.className }
      $graphName.setupNodes()
      ${ metaFactor.classDef }
      val result:StructuredFactor[${ meta.argType }] = new ${ metaFactor.className }($structName)
      $graphName.build()
      val _factorieWeights = $factorieWeights
      $graphName.weights = _factorieWeights
      result
    """
    (context.resetLocalAttrs(code), metaFactor)
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
                          potential: T => Double): StructuredFactor[T] = macro structuredFactorImpl[T]

  def structuredLinearFactor[T](sampleSpace: Iterable[T],
                                potential: Wolfe.Vector => T => Double): Wolfe.Vector => StructuredFactor[T] = macro structuredLinearFactorImpl[T]


  def structuredFactorImpl[T: c.WeakTypeTag](c: Context)(sampleSpace: c.Expr[Iterable[T]],
                                                         potential: c.Expr[T => Double]) = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with MetaStructuredFactors[c.type]

    val q"($arg) => $rhs" = helper.unwrapSingletonBlocks(potential.tree)
    val (code, _) = helper.structuredLinearFactorCode(sampleSpace.tree, arg, rhs)
    c.Expr[StructuredFactor[T]](code)
  }


  def structuredLinearFactorImpl[T: c.WeakTypeTag](c: Context)(sampleSpace: c.Expr[Iterable[T]],
                                                               potential: c.Expr[Wolfe.Vector => T => Double]) = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with MetaStructuredFactors[c.type]

    val q"($param) => ($arg) => $rhs" = helper.unwrapSingletonBlocks(potential.tree)
    val (code, meta) = helper.structuredLinearFactorCode(sampleSpace.tree, arg, rhs)
    if (!meta.weightVector.exists(_.symbol == param.symbol))
      c.error(c.enclosingPosition,
        s"Weight vector of linear model doesn't match parameter: ${ meta.weightVector } != ${ param.symbol }")

    val function = q"($param => $code)"
    c.Expr[Wolfe.Vector => StructuredFactor[T]](function)
    /*

    val root = helper.rootMatcher(arg.symbol, q"$structName",meta)
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
        val factorieWeights = ml.wolfe.FactorieConverter.toFactorieDenseVector(weights,_index)
        $graphName.weights = factorieWeights
        result
      }
    """
     */
  }


}