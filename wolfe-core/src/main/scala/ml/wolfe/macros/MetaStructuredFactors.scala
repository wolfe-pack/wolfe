package ml.wolfe.macros

import scala.collection.mutable
import scala.reflect.macros.{TypecheckException, Context}
import ml.wolfe.Wolfe
import ml.wolfe.util.CachedPartialFunction

/**
 * Functionality for creating structured factors.
 *
 * @tparam C type of context.
 * @author Sebastian Riedel
 */
trait MetaStructuredFactors[C <: Context] extends MetaStructures[C]
                                                  with CodeOptimizer[C]
                                                  with MetaAtomicStructuredFactors[C] {


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
      case (dom :: Nil, id :: Nil) => q"Range(0,$dom.length).$lastOp(($id:Int) => $body)"
      case (dom :: domTail, id :: idTail) =>
        val inner = tupleProcessor(domTail, idTail, body, op)
        q"Range(0,$dom.length).$op(($id:Int) => $inner)"
      case _ => sys.error("shouldn't happen")
    }


  case class MetaFirstOrderSumFactor(domains: List[Tree], obj: Tree,
                                     info: FactorGenerationInfo) extends MetaStructuredFactor {

    import info._

    val className                  = newTypeName(context.fresh("FirstOrderSumFactor"))
    val q"(..$objArgs) => $objRhs" = obj
    //domains may contain references to values in the sample space.
    val injectedDoms               = domains.map(injectStructure(_, matcher))
    val keyDomNames                = List.fill(domains.size)(newTermName(context.fresh("qSumDom")))
    val keyDomSizes                = keyDomNames.map(k => q"$k.length")
    val tmpNames                   = Range(0, domains.size).map(i => newTermName(context.fresh("i" + i))).toList
    val tmpIds                     = tmpNames.map(Ident(_))
    val domainDefs                 = for ((d, n) <- injectedDoms zip keyDomNames) yield q"val $n = $d.toArray"
    val ownParams                  = q"val structure:${ structure.className }" :: constructorArgs
    val childParams                = constructorArgs ::: tmpNames.map(id => q"val $id:Int")
    val childArgs                  = (q"structure" :: constructorArgs.map(a => q"${ a.name }")) ::: tmpIds
    val substitutedObj             = replaceArgumentWithOwnArg(objRhs)
    def replaceArgumentWithOwnArg(tree: Tree): Tree = {
      transform(tree, {
        case i: Ident if objArgs.exists(_.symbol == i.symbol) =>
          val index = objArgs.indexWhere(_.symbol == i.symbol)
          val replacement = q"${ keyDomNames(index) }(${ tmpIds(index) })"
          replacement
      })
    }

    val child = metaStructuredFactor(info.copy(
      potential = objRhs,
      constructorArgs = childParams,
      transformer = info.transformer.andThen(replaceArgumentWithOwnArg)
    ))

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

  trait MetaGenericStructuredFactor extends MetaStructuredFactor {
    val info: FactorGenerationInfo

    import info._

    lazy val className       = newTypeName(context.fresh("GenericStructuredFactor"))
    lazy val arguments       = distinctTrees(structures(potential, matcher).filterNot(_.meta.observed).map(_.structure))
    lazy val nodesPerArg     = arguments.map(a => q"$a.nodes()")
    lazy val nodes           = q"""Iterator(..$nodesPerArg).flatMap(identity)"""
    lazy val constructorArgs = q"val structure:${ structure.className }" :: info.constructorArgs

    def children = Nil
    def classDef = q"""
      final class $className(..$constructorArgs) extends ml.wolfe.macros.StructuredFactor[${ structure.argType }] {
        import ml.wolfe.FactorGraph._
        val nodes:Array[Node] = $nodes.toArray
        val factor = graph.addFactor()
        val edges = nodes.view.zipWithIndex.map(p => graph.addEdge(factor,p._1,p._2)).toArray
        factor.potential = null
        def factors = Iterator(factor)
        def arguments = List(..$arguments)
      }
    """

  }

  def tailorMadePotential(info: FactorGenerationInfo, argss: List[List[Tree]], annotation: Annotation) = {
    import info._
    case class EdgeData(nodeDefs:Seq[Tree], edge:Tree)

    val iterableType = typeOf[Iterable[_]].typeSymbol
    def isIterable(tpe:Type) = tpe.baseClasses.contains(iterableType)

    val argData:Seq[EdgeData] = argss reduce (_ ++ _) map { a:Tree =>
      if(structures(a, matcher) == Nil) {
        // Argument is a constant
        val nodeName = newTermName(context.fresh("extraNode"))
        val nodeDefs = a match {
          case Literal(Constant(_)) => Seq(q"val $nodeName = graph.addDiscreteNodeWithDomain(Array($a), ${a.toString()})")
        }
        EdgeData(nodeDefs, q"graph.addEdge(factor, $nodeName)")

      } else a match {

        case q"${s:Tree}.apply(${i:Tree})" if isIterable(s.tpe) && structures(s, matcher).isEmpty && matcher(i) != None =>
          //TODO: structure must be atomic! Maybe make AtomicStructure with .node for idxNode
          val choiceFactor = newTermName(context.fresh("choiceFactor"))
          val chosenNode = newTermName(context.fresh("chosenNode"))
          val choiceNodes = newTermName(context.fresh("choiceNodes"))

          val matchedIdx = matcher(i).get
          val idxNode = q"${matchedIdx.structure}.node"
          val nodeDefs = Seq(
             q"val $chosenNode:Node = graph.addDiscreteNodeWithDomain($s.toArray, ${s.toString() + "(" + i.toString + ")"})",
             q"val $choiceNodes:Seq[Node] = (0 until $s.length).map( i => graph.addDiscreteNodeWithDomain(Array($s(i)), $s(i).toString) )",
             q"val $choiceFactor = graph.addFactor()",
             q"val p = ${matchedIdx.structure}",
             q"""
                 $choiceFactor.potential = new ml.wolfe.fg.ChoicePotential(
                  graph.addEdge($choiceFactor, $chosenNode),
                  $choiceNodes.map(n => graph.addEdge($choiceFactor, n)),
                  graph.addEdge($choiceFactor, $idxNode)
                )
             """
             )

          EdgeData(nodeDefs, q"graph.addEdge(factor, $chosenNode)")

        case _ =>
          val injected = injectStructure(a, matcher, t => q"$t.createEdges(factor)", ignoreTermsWithFunctionArgs = false)
          val removeTypes = transform(injected, {
            case Apply(TypeApply(f, _), funArgs) => Apply(f, funArgs)
            case TypeApply(s: Select, _) => s
          })
          val reset = context.resetAllAttrs(removeTypes)
          EdgeData(Seq(), reset)

      }
    }





    val argumentStructures = distinctTrees(structures(potential, matcher).map(_.structure))
    val argumentEdges = argData.map(_.edge)
    val nodeDefs = argData.flatMap(_.nodeDefs)
    val createPotential = q"${ annotation.scalaArgs.head }((..$argumentEdges))"

    val nameOfClass = newTypeName(context.fresh("GenericStructuredFactor"))
    val constructorArgs = q"val structure:${ structure.className }" :: info.constructorArgs

    //todo: get ..$argumentStructures back into arguments

    val classDefinition = q"""
      final class $nameOfClass(..$constructorArgs) extends ml.wolfe.macros.StructuredFactor[${ structure.argType }] {
        import ml.wolfe.FactorGraph._
        val factor = graph.addFactor()
        ..$nodeDefs
        factor.potential = $createPotential
        def factors = Iterator(factor)
        def arguments = List(..$argumentStructures)
      }
    """
    new MetaStructuredFactor {
      def children = Nil
      def className = nameOfClass
      def classDef = classDefinition
    }

  }


  def variablesContainArgument(obj: Tree, matcher: Tree => Option[StructurePointer]) = {
    val Function(List(arg), rhs) = normalize(obj)
    //find other sums in in objective and collect their arguments. These don't count as free variables
    //that break the structure
    val allSumArgs: List[Symbol] = rhs.collect({
      case Sum(BuilderTrees(_, _, sumObj, _, _)) =>
        val Function(List(sumArg), _) = normalize(sumObj)
        sumArg.symbol
    })
    val structs = structures(rhs, matcher, allSumArgs.toSet)
    val variables = distinctTrees(structs.filterNot(_.meta.observed).map(_.structure))
    variables.exists(_.exists(_.symbol == arg.symbol))
  }

  def variablesContainArgumentInline(obj: Tree, matcher: Tree => Option[StructurePointer]):Boolean = {
    val result = variablesContainArgument(obj,matcher)
    if (result) result else inlineOnce(obj) match {
      case Some(inlined) => variablesContainArgumentInline(inlined,matcher)
      case None => false
    }
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
          //todo: this shouldn't really be failing, but it does when the terms contain generated terms
          try {
            val typed = context.typeCheck(context.resetLocalAttrs(newSum))
            Some(arg2 -> typed)
          } catch {
            case e:TypecheckException => None
          }
        case (Sum(_), Sum(_)) => None
        case (p1, p2) if inlineOnce(p1).isEmpty && inlineOnce(p2).isEmpty =>
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
          case Some((orig, merged)) =>
            result.map(a => if (a == orig) merged else a)
          case None =>
            arg :: result
        }
    }
    mergeResult.reverse
    //args
  }

  case class FactorGenerationInfo(potential: Tree,
                                  structure: MetaStructure,
                                  matcher: Tree => Option[StructurePointer],
                                  constructorArgs: List[ValDef] = Nil,
                                  linearModelInfo: LinearModelInfo,
                                  linear: Boolean = false,
                                  transformer: Tree => Tree = identity[Tree],
                                  expectations: Boolean = false)


  def metaStructuredFactor(info: FactorGenerationInfo): MetaStructuredFactor = {
    import info._
    //    val simplified = unwrapSingletonBlocks(potential)
    val simplified = simplifyBlock(unwrapSingletonBlocks(potential))
    simplified match {
      case Sum(BuilderTrees(dom, filter, obj, _, _)) =>
        require(filter == EmptyTree)
        //check whether we need to further factorize (only if the objective argument is part of the variables)
        if (!variablesContainArgumentInline(obj, matcher))
          atomic(info)
        else
          MetaFirstOrderSumFactor(List(dom), obj, info)
      case Apply(f, args) if f.symbol.annotations.exists(_.tpe.typeSymbol == wolfeSymbols.atomic) =>
        atomic(info)
      case q"$f(...$argss)" if f.symbol.annotations.exists(_.tpe.typeSymbol == wolfeSymbols.potential)  =>
        tailorMadePotential(info, argss, f.symbol.annotations.find(_.tpe.typeSymbol == wolfeSymbols.potential).get)
      case FlattenedPlus(args) =>
        val merged = mergeSumArgs(args, matcher)
        val metaStructs = merged.map(arg =>
          metaStructuredFactor(info.copy(potential = arg)))
        MetaSumFactor(merged, metaStructs, structure, constructorArgs)
      case ApplyMinus(arg1,arg2) =>
        val merged = mergeSumArgs(List(arg1, q"-1 * $arg2"), matcher)
        val metaStructs = merged.map(arg =>
          metaStructuredFactor(info.copy(potential = arg)))
        MetaSumFactor(merged, metaStructs, structure, constructorArgs)
      case Dot(arg1, arg2) if structures(arg1, matcher).isEmpty =>
        val linearFactor = metaStructuredFactor(FactorGenerationInfo(arg2, structure, matcher, constructorArgs, linearModelInfo, true))
        WithWeightVector(linearFactor, arg1)
      case Dot(arg2, arg1) if structures(arg1, matcher).isEmpty =>
        val linearFactor = metaStructuredFactor(FactorGenerationInfo(arg2, structure, matcher, constructorArgs, linearModelInfo, true))
        WithWeightVector(linearFactor, arg1)
      case _ => inlineOnce(potential) match {
        case Some(inlined) =>
          metaStructuredFactor(info.copy(potential = inlined))
        case None =>
          atomic(info)
      }
    }
  }

  def structuredLinearFactorCode(sampleSpace: Tree, arg: Tree, rhs: Tree) = {
    val structName = newTermName("structure")
    val graphName = newTermName("_graph")
    val meta = metaStructure(sampleSpace)
    val root = rootMatcher(arg.symbol, q"$structName", meta)
    val matcher = meta.matcher(root)
    val metaFactor = metaStructuredFactor(FactorGenerationInfo(rhs, meta, matcher, linearModelInfo = LinearModelInfo(q"_index")))
    val factorieWeights = metaFactor.weightVector.map(
      w => q"ml.wolfe.FactorieConverter.toFactorieDenseVector($w,_index)"
    ).getOrElse(q"new ml.wolfe.DenseVector(0)")
    val structureClass = meta.classDef(graphName)
    val code = q"""
      val _index = new ml.wolfe.DefaultIndex
      val $graphName = new ml.wolfe.FactorGraph
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

  }






  // -----------------------------------------------------------------------

  def shortCode(c:Context)(t:c.Tree):c.Tree = {
    import c.universe._

    t match {
      case q"ml.wolfe.Wolfe.${x:TermName}" => q"${x.toString}"
      case q"ml.wolfe.macros.OptimizedOperators.${x:TermName}" => q"${x.toString}"
      case q"!( $x )" => q""" "!(" + ${shortCode(c)(x)} + ")" """
      case q"$x.||" => q""" ${shortCode(c)(x)} + " || " """
      case q"$x.^" => q"""${shortCode(c)(x)} + " ^ " """
      case q"$x.&&" => q"""${shortCode(c)(x)} + " && " """
      case q"$x.==" => q"""${shortCode(c)(x)} + " == " """
      case q"$x.>" => q"""${shortCode(c)(x)} + " > " """
      case q"$x.<" => q"""${shortCode(c)(x)} + " < " """
      case q"$x.>=" => q"""${shortCode(c)(x)} + " >= " """
      case q"$x.<=" => q"""${shortCode(c)(x)} + " <= " """
      case q"$x.+" => q"""${shortCode(c)(x)} + " + " """
      case q"$x.-" => q"""${shortCode(c)(x)} + " - " """
      case q"$x.*" => q"""${shortCode(c)(x)} + " * " """
      case q"$x./" => q"""${shortCode(c)(x)} + " / " """
      case q"$x.apply" => shortCode(c)(x)
      case q"qSumDom1($x)" => t
      case q"qSumDom2($x)" => t
      case q"qSumDom3($x)" => t
      case q"qSumDom4($x)" => t
      case q"qSumDom5($x)" => t
      case q"qSumDom6($x)" => t
      case q"${x:Select}" =>  q"""${shortCode(c)(x.qualifier)} + "." + ${x.name.toString} """
      case q"${f:Select}($arg)" => q"""${shortCode(c)(f)} + "(" + ${shortCode(c)(arg)} + ")" """
      case _ => q"${t.toString}"
    }
  }
}