package scalapplcodefest.macros

import scala.reflect.macros.Context
import scala.collection.mutable
import scalapplcodefest.MPGraph
import scalapplcodefest.MPGraph.Factor

/**
 * @author Sebastian Riedel
 */
trait StructureHelper[C <: Context] {
  this: MacroHelper[C] =>

  import context.universe._


  case class MetaStructuredGraph(data: Tree, where: Tree, obj: Tree, weightMatcher: Tree => Boolean = _ => false) {

    val graphClassName  = newTypeName(context.fresh("StructuredGraph"))
    val mpGraphName     = newTermName(context.fresh("mpGraph"))
    val structureName   = newTermName(context.fresh("structure"))
    val objTreeName     = newTermName(context.fresh("objTree"))
    val predTreeName    = newTermName(context.fresh("predTree"))
    val nodeCountName   = newTermName(context.fresh("nodeCount"))
    val nextNodeIndex   = newTermName(context.fresh("nextNodeIndex"))
    val vectorIndexName = newTermName(context.fresh("vectorIndex"))
    val init            = List(q"val $mpGraphName = new MPGraph()")

    val factorieConverter = q"scalapplcodefest.sbt.FactorieConverter"


    val normalizedObj  = betaReduce(replaceMethods(simplifyBlocks(obj), defs))
    val normalizedPred = betaReduce(replaceMethods(simplifyBlocks(where), defs))

    val Function(List(objVarDef@ValDef(_, objVarName, _, _)), rootObj)    = normalizedObj
    val Function(List(predVarDef@ValDef(_, predVarName, _, _)), rootPred) = normalizedPred

    //inline the data tree
    //    val inlinedData = transform(data, {
    //      case i@Ident(name) => vals.getOrElse(name, i)
    //      case t => t
    //    })
    val inlinedData = replaceVals(betaReduce(replaceMethods(simplifyBlocks(data), defs)), valDefs)

    val checked = context.typeCheck(inlinedData)


    //the domain/sample space. May expand the domain if implicits were involved.
    //todo: this may be possible without the domain expansion trick by using context.typeCheck
    val rootDom = DomainExpansions.findByPosition(inlinedData.pos.startOrPoint, inlinedData.pos.source) match {
      case Some(expansion) => expansion.term.tree.asInstanceOf[Tree]
      case other => inlinedData
    }

    //meta information about the root structure class.
    val metaStructure = createMetaStructure(this, checked)

    val objArgMatcher  = createRootMatcher(objVarName, Ident(structureName))(_)
    val predArgMatcher = createRootMatcher(predVarName, Ident(structureName))(_)

    val normalized = betaReduce(replaceMethods(rootObj, defs))

    val objFactorTree = createMetaFactorTree(normalized, metaStructure.matcher(objArgMatcher, objArgMatcher))


    //setting observed nodes and reduce their domains
    val predObservationSetup = observationSetup(this, rootPred, metaStructure.matcher(predArgMatcher, predArgMatcher))

    //turn the predicate into an objective
    val predObj = predObservationSetup.remainder match {
      case EmptyTree => EmptyTree
      case setup => q"math.log(I($setup))"
    }

    //setting up the factors from the predicate
    val predFactorTree = createMetaFactorTree(predObj, metaStructure.matcher(predArgMatcher, predArgMatcher))

    //all structure class definitions
    val classDefs = metaStructure.all.map(_.classDef)

    val domDefs =
    //      metaStructure.allDomainDefs ++
      objFactorTree.all.flatMap(_.domainDefs) ++
      predFactorTree.all.flatMap(_.domainDefs)

    //        ..$domDefs

    val classDef = context.resetAllAttrs( q"""
      class $graphClassName(val $vectorIndexName: scalapplcodefest.Index) {
        import scalapplcodefest._
        import scalapplcodefest.Wolfe._
        import scalapplcodefest.MPGraph._
        import scalapplcodefest.macros._
        import scala.language.reflectiveCalls


        ${metaStructure.classDef}

        val $mpGraphName = new MPGraph()

        val $structureName = new ${metaStructure.className}

        ${predObservationSetup.setup}

        $mpGraphName.setupNodes()

        ${objFactorTree.setup}
        ${predFactorTree.setup}

        $mpGraphName.build()

        println("Nodes:   " + $mpGraphName.nodes.size)
        println("Factors: " + $mpGraphName.factors.size)

      }
    """)

    trait MetaFactorTree {
      def children: List[MetaFactorTree]
      def all: List[MetaFactorTree] = this :: children.flatMap(_.all)
      def setup: Tree
      def domainDefs: List[ValDef] = Nil
    }

    def createMetaFactorTree(potential: Tree, matchStructure: Tree => Option[Tree], linear:Boolean = false): MetaFactorTree = potential match {
      case EmptyTree => MetaEmptyFactor
      case q"${_}.log(${FlatProduct(args)})" =>
        val children = args.map(a => createMetaFactorTree(q"log($a)", matchStructure, linear))
        MetaPropositionalSum(args, children)
      case FlatSum(args) =>
        val children = args.map(a => createMetaFactorTree(a, matchStructure, linear))
        MetaPropositionalSum(args, children)
      case QuantifiedSum(qdom, qpred, qobj) =>
        val q"(..$x) => $rhs" = qobj
        MetaQuantifiedSum(x, List(qdom), qpred, rhs, matchStructure, linear)
      //todo: should allow weight on arg1 as well
      case DotProduct(arg1, arg2) if weightMatcher(arg2) && arg1.forAll(!weightMatcher(_)) =>
        createMetaFactorTree(arg1, matchStructure, true)
//        MetaLinearFactorLeaf(arg1, matchStructure)
      case DotProduct(_, _) =>
        val dist = distributeDots(potential)
        if (dist == potential) MetaFactorLeaf(potential, matchStructure) else createMetaFactorTree(dist, matchStructure, linear)
      case _ =>
        if (linear) MetaLinearFactorLeaf(potential, matchStructure) else MetaFactorLeaf(potential, matchStructure)
    }

    case object MetaEmptyFactor extends MetaFactorTree {
      def children = Nil
      val setup = EmptyTree
    }

    case class MetaPropositionalSum(args: List[Tree], children: List[MetaFactorTree]) extends MetaFactorTree {
      val setupChildren = for (c <- children) yield q"${c.setup}"
      val fieldIds      = for (i <- children.indices) yield q"${newTermName("arg" + i)}"
      val setup         = q"{..$setupChildren}"

    }

    case class MetaQuantifiedSum(args: List[ValDef], doms: List[Tree], pred: Tree, obj: Tree,
                                 matchStructure: Tree => Option[Tree], linear:Boolean) extends MetaFactorTree {
      val injectedDoms   = doms.map(injectStructure(_,matchStructure))
      val keyDomNames    = List.fill(doms.size)(newTermName(context.fresh("qSumDom")))
      val keyDomSizes    = keyDomNames.map(k => q"$k.length")
      val tmpNames       = Range(0, doms.size).map(i => newTermName("i" + i)).toList
      val tmpIds         = tmpNames.map(Ident(_))
      val substitutedObj = transform(obj, {
        case Ident(name) if args.exists(_.name == name) =>
          val index = args.indexWhere(_.name == name)
          val replacement = q"${keyDomNames(index)}(${tmpIds(index)})"
          replacement
      })

      override val domainDefs = for ((d, n) <- injectedDoms zip keyDomNames) yield q"val $n = $d.toArray"

      val child         = createMetaFactorTree(substitutedObj, matchStructure, linear)
      val setupChild    = q"${child.setup}"
      val setupChildren = tupleProcessor(keyDomNames, tmpNames, setupChild, newTermName("foreach"), newTermName("foreach"))
      val setup         = q"{ ..$domainDefs; $setupChildren}"
      def children = List(child)



    }

    trait Leaf {
      def term: Tree
      def matchStructure: Tree => Option[Tree]
      val arguments   = distinctTrees(structures(term, matchStructure))
      val nodesPerArg = arguments.map(a => q"$a.nodes()")
      val nodes       = q"""Iterator(..$nodesPerArg).flatMap(identity)"""
      val cleaned     = context.resetAllAttrs(term)
      val injected    = injectStructure(cleaned, matchStructure)
      def children = Nil
    }

    case class MetaFactorLeaf(term: Tree, matchStructure: Tree => Option[Tree]) extends MetaFactorTree with Leaf {
      val perSetting = q"""
        //println(nodes.map(_.setting).mkString(","))
        settings(settingIndex) = nodes.map(_.setting)
        scores(settingIndex) = $injected
        settingIndex += 1
      """

      val loop = loopSettings(arguments) {perSetting}

      val setup = q"""
        val nodes:Array[Node] = $nodes.toArray
        val dims = nodes.map(_.dim)
        val settingsCount = dims.product
        val settings = Array.ofDim[Array[Int]](settingsCount)
        val scores = Array.ofDim[Double](settingsCount)
        var settingIndex = 0
        $loop
        val factor = $mpGraphName.addTableFactor(scores, settings, dims)
        nodes.view.zipWithIndex.foreach(p => $mpGraphName.addEdge(factor,p._1,p._2))
      """
    }

    case class MetaLinearFactorLeaf(term: Tree, matchStructure: Tree => Option[Tree]) extends MetaFactorTree with Leaf {
      val perSetting = q"""
        //println(nodes.map(_.setting).mkString(","))
        settings(settingIndex) = nodes.map(_.setting)
        vectors(settingIndex) = $factorieConverter.toFactorieSparseVector($injected,$vectorIndexName)
        settingIndex += 1
      """

      val loop = loopSettings(arguments) {perSetting}

      val setup = q"""
        val nodes:Array[Node] = $nodes.toArray
        val dims = nodes.map(_.dim)
        val settingsCount = dims.product
        val settings = Array.ofDim[Array[Int]](settingsCount)
        val vectors = Array.ofDim[scalapplcodefest.Vector](settingsCount)
        var settingIndex = 0
        $loop
        val factor = $mpGraphName.addLinearFactor(vectors, settings, dims)
        nodes.view.zipWithIndex.foreach(p => $mpGraphName.addEdge(factor,p._1,p._2))
      """
    }


  }

  def createMetaStructure(metadata: MetaStructuredGraph, domain: Tree): MetaStructure = {

    def caseClassStructure(constructor: Tree, sets: List[Tree]) = {
      val tpe: Type = constructor.tpe
      //todo: we should find the structure of the case class based on the type information, not collected definitions
      val members = tpe.members
      val caseClassName = tpe.typeSymbol.name.toTypeName
      val caseClass = classes.getOrElse(caseClassName, null)
      val q"case class $className(..$fields)" = caseClass
      val subtypes = sets.map(createMetaStructure(metadata, _))
      MetaCaseClassStructure(tpe, fields, subtypes)
    }

    domain match {
      //todo: after typeCheck there may be a different structure here, need to catch it.
      case q"$all[..${_}]($unwrap[..${_}]($constructor))($cross(..$sets))"
        if all.symbol.name.encoded.startsWith("all") && unwrap.symbol.name.encoded.startsWith("unwrap") =>
        caseClassStructure(constructor, sets)
      case q"$all[..${_}]($constructor)($set)"
        if all.symbol.name.encoded.startsWith("all") =>
        caseClassStructure(constructor, List(set))
      case q"${_}.seqs[..${_}]($srcSeq.map[..${_}](($arg) => $argDom)(${_}))" =>
        val TypeRef(_, _, List(argType)) = domain.tpe
        MetaSeqVarDomStructure(argType, srcSeq, arg.name, createMetaStructure(metadata, argDom))
      case q"scalapplcodefest.Wolfe.Pred[${_}]($keyDom)" =>
        val keyDoms = keyDom match {
          case q"scalapplcodefest.Wolfe.$cross[..${_}](..$doms)" => doms
          case _ => List(keyDom)
        }
        val valueDom = q"scalapplcodefest.Wolfe.bools"
        val tped = context.typeCheck(valueDom)
        val TypeRef(_, _, List(argType)) = domain.tpe
        MetaFunStructure(argType, keyDoms, createMetaStructure(metadata, tped))
      case _ => MetaAtomicStructure(domain, metadata)
    }
  }

  trait MetaStructure {
    def className: TypeName
    def domainDefs: List[ValDef]
    def allDomainDefs = all.flatMap(_.domainDefs)
    def classDef: ClassDef
    def argType: Type
    def children: List[MetaStructure]
    def all: List[MetaStructure] = this :: children.flatMap(_.all)
    def matcher(parent: Tree => Option[Tree], result: Tree => Option[Tree]): Tree => Option[Tree]

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

  def curriedArguments(indices: List[Tree], result: Tree = q"subStructures"): Tree = indices match {
    case Nil => result
    case i :: tail => curriedArguments(tail, q"$result($i)")
  }


  case class MetaSeqVarDomStructure(argType: Type, domSeqSrc: Tree, argName: TermName, elementStructure: MetaStructure) extends MetaStructure {
    def children = List(elementStructure)
    def domainDefs = Nil

    def matcher(parent: Tree => Option[Tree], result: Tree => Option[Tree]): Tree => Option[Tree] = {
      def matchApp(tree: Tree) = {
        def replace(f: Tree, arg: Tree) = parent(f) match {
          case Some(parentStructure) =>
            val substructure = q"$parentStructure.subStructures($arg)"
            Some(substructure)
          case _ => None
        }
        tree match {
          case q"$f.apply($arg)" => replace(f, arg)
          case q"$f($arg)" => replace(f, arg)
          case _ => None
        }
      }
      elementStructure.matcher(matchApp, (t: Tree) => matchApp(t).orElse(result(t)))
    }

    //todo: get rid of reflective call by allowing/requiring constructors for structures that take the domain as argument
    val className = newTypeName(context.fresh("SeqStructure"))
    val classDef  = q"""
      final class $className extends Structure[$argType] with HasSettingIterator[$argType]{
        val subStructures = $domSeqSrc.map($argName => {
          ${elementStructure.classDef}
          new ${elementStructure.className}
        }).toArray
        def nodes() = subStructures.iterator.flatMap(_.nodes())
        def newIterator() = Structure.settingsIterator(subStructures.toList)()
        def value() = subStructures.view.map(_.value()).force.toSeq
        def setToArgmax() {subStructures.foreach(_.setToArgmax())}
        def observe(s:$argType) {
          Range(0,s.size).foreach( i => subStructures(i).observe(s(i)))
        }
      }
    """

  }

  case class MetaFunStructure(tpe: Type, keyDoms: List[Tree], valueType: MetaStructure) extends MetaStructure {
    val keyDomNames   = List.fill(keyDoms.size)(newTermName(context.fresh("funKeyDom")))
    val keyIndexNames = List.fill(keyDoms.size)(newTermName(context.fresh("funKeyIndex")))
    val tmpNames      = Range(0, keyDoms.size).map(i => newTermName("i" + i)).toList
    val tmpIds        = tmpNames.map(Ident(_))
    val argType       = tpe
    val argTypeName   = tpe.typeSymbol.name.toTypeName
    val tupleArgs     = for ((i, k) <- tmpNames zip keyDomNames) yield q"$k($i)"
    val invTupleArgs  = for ((i, k) <- tmpNames zip keyIndexNames) yield q"$k($i)"
    val tuple         = if (tupleArgs.size == 1) tupleArgs.head else q"(..$tupleArgs)"
    val keyDomSizes   = keyDomNames.map(k => q"$k.length")
    val className     = newTypeName(context.fresh("FunStructure"))
    val domDefs       = for ((d, n) <- keyDoms zip keyDomNames) yield q"val $n = $d.toArray"
    val indexDefs     = for ((d, i) <- keyDomNames zip keyIndexNames) yield q"val $i = $d.zipWithIndex.toMap"
    val domainDefs    = domDefs ++ indexDefs


    def children = List(valueType)

    def matcher(parent: Tree => Option[Tree], result: Tree => Option[Tree]): Tree => Option[Tree] = {
      def matchApp(tree: Tree) = {
        def replace(f: Tree, args: List[Tree]) = parent(f) match {
          //        case Apply(f, args) => parent(f) match {
          case Some(parentStructure) =>
            val asIndices = for ((a, i) <- args zip keyIndexNames) yield q"$i($a)"
            val substructure = curriedArguments(asIndices, q"$parentStructure.subStructures")
            Some(substructure)
          case _ => None
        }
        tree match {
          case q"$f.apply(..$args)" => replace(f, args)
          case q"$f(..$args)" => replace(f, args)
          case _ => None
        }
      }
      valueType.matcher(matchApp, (t: Tree) => matchApp(t).orElse(result(t)))
    }


    def substructureIterator(count: Int, result: Tree = q"subStructures.iterator"): Tree = count match {
      case 1 => q"$result"
      case n => substructureIterator(n - 1, q"$result.flatMap(_.iterator)")
    }


    val mappings = tupleProcessor(keyDomNames, tmpNames, q"$tuple -> ${curriedArguments(tmpIds)}.value")

    val observeSubStructure = q"${curriedArguments(tmpIds)}.observe(value($tuple))"

    val classDef = q"""
      final class $className extends Structure[$argType]{
        ${valueType.classDef}
        ..$domainDefs
        private var iterator:Iterator[Unit] = _
        val subStructures = Array.fill(..$keyDomSizes)(new ${valueType.className})
        def subStructureIterator() = ${substructureIterator(keyDoms.size)}
        def nodes() = subStructureIterator().flatMap(_.nodes())
        def resetSetting() { iterator = Structure.settingsIterator(subStructureIterator().toList)()}
        def hasNextSetting = iterator.hasNext
        def nextSetting = iterator.next
        def setToArgmax() {subStructureIterator().foreach(_.setToArgmax())}
        def value() = $mappings.toMap
        def observe(value:$argType) {
          ${tupleProcessor(keyDomNames, tmpNames, observeSubStructure, newTermName("foreach"), newTermName("foreach"))}
        }
      }
    """

  }

  case class MetaCaseClassStructure(tpe: Type, fields: List[ValDef], types: List[MetaStructure]) extends MetaStructure {
    val fieldsAndTypes  = fields zip types
    val subClassDefs    = types.map(_.classDef)
    val argTypeName     = tpe.typeSymbol.name.toTypeName
    val className       = newTypeName(context.fresh(tpe.typeSymbol.name.encoded + "Structure"))
    val structureFields = for ((ValDef(mods, name, _, _), t) <- fields zip types) yield
      q"val $name = new ${t.className}"
    val fieldIds        = fields.map(f => Ident(f.name))
    val fieldValues     = fieldIds.map(i => q"$i.value()")
    val observeFields   = fields.map(f => q"${f.name}.observe(value.${f.name})")
    val classDef        = q"""
      final class $className extends Structure[$argTypeName] {
        ..$subClassDefs
        ..$structureFields
        private var iterator:Iterator[Unit] = _
        def fields:Iterator[Structure[Any]] = Iterator(..$fieldIds)
        def value():$argTypeName = new $argTypeName(..$fieldValues)
        def nodes():Iterator[Node] = fields.flatMap(_.nodes())
        def resetSetting() { iterator = Structure.settingsIterator(List(..$fieldIds).reverse)()}
        def hasNextSetting = iterator.hasNext
        def nextSetting = iterator.next
        def setToArgmax() {fields.foreach(_.setToArgmax())}
        def observe(value:$argTypeName) { ..$observeFields }
      }
    """
    def children = types
    def argType = tpe
    def domainDefs = Nil

    def matcher(parent: Tree => Option[Tree], result: Tree => Option[Tree]): Tree => Option[Tree] = {
      def matchField(field: ValDef)(tree: Tree): Option[Tree] = tree match {
        case q"$data.$f" if field.name == f => for (s <- parent(data)) yield q"$s.$f"
        case _ => None
      }
      val fieldMatchers = fieldsAndTypes.map({case (f, t) => t.matcher(matchField(f), matchField(f))})
      def firstMatch(matchers: List[Tree => Option[Tree]]): Tree => Option[Tree] = matchers match {
        case Nil => result
        case head :: tail => (t: Tree) => head(t).orElse(firstMatch(tail)(t))
      }
      firstMatch(fieldMatchers)
    }


  }

  case class MetaAtomicStructure(domain: Tree, meta: MetaStructuredGraph) extends MetaStructure {
    val domName                      = newTermName(context.fresh("atomDom"))
    val indexName                    = newTermName(context.fresh("atomIndex"))
    val className                    = newTypeName(context.fresh("AtomicStructure"))
    val TypeRef(_, _, List(argType)) = domain.tpe
    val argTypeName                  = argType.typeSymbol.name.toTypeName
    val domainDefs                   = List(
      q"val $domName = $domain.toArray",
      q"val $indexName = $domName.zipWithIndex.toMap")
    def children = Nil
    val classDef = q"""
      final class $className extends Structure[$argType] {
        ..$domainDefs
        val node = ${meta.mpGraphName}.addNode($domName.length)
        private def updateValue() {node.value = node.domain(node.setting)}
        def value() = $domName(node.value)
        def nodes() = Iterator(node)
        def resetSetting() {node.setting = -1}
        def hasNextSetting = node.setting < node.dim - 1
        def nextSetting() = {node.setting += 1; updateValue()}
        def setToArgmax() { node.setting = MoreArrayOps.maxIndex(node.b); updateValue()}
        final def observe(value:$argType) {
          val index = $indexName(value)
          node.domain = Array(index)
          node.dim = 1
        }
      }
    """

    def matcher(parent: Tree => Option[Tree], result: Tree => Option[Tree]): Tree => Option[Tree] = result


  }

  def createRootMatcher(name: TermName, rootStructure: Tree)(tree: Tree): Option[Tree] = tree match {
    //    case id: Ident if id.tpe =:= typ => Some(rootStructure)
    case Ident(n) if n == name => Some(rootStructure)
    case _ => None
  }

  case class ObservationSetup(setup: Tree, remainder: Tree)

  def observationSetup(metadata: MetaStructuredGraph, pred: Tree, matchStructure: Tree => Option[Tree]): ObservationSetup = pred match {
    case q"$x == $value" => matchStructure(x) match {
      case Some(structure) => ObservationSetup(q"$structure.observe($value)", EmptyTree)
      case _ => ObservationSetup(EmptyTree, pred)
    }
    case x => matchStructure(x) match {
      case Some(structure) =>
        ObservationSetup(q"$structure.observe(true)", EmptyTree)
      case _ =>
        ObservationSetup(EmptyTree, pred)
    }
  }


  def structures(tree: Tree, matchStructure: Tree => Option[Tree]): List[Tree] = {
    var result: List[Tree] = Nil
    val traverser = new Traverser with WithFunctionStack {
      override def traverse(tree: Tree) = {
        pushIfFunction(tree)
        val tmp = matchStructure(tree) match {
          case Some(structure) if !hasFunctionArgument(tree) =>
            result ::= structure
          case _ =>
            super.traverse(tree)
        }
        popIfFunction(tree)
        tmp
      }
    }
    traverser traverse tree
    result
  }

  trait WithFunctionStack {

    private val functionStack = new mutable.Stack[Function]()

    def pushIfFunction(tree: Tree) {
      tree match {
        case f: Function => functionStack.push(f)
        case _ =>
      }
    }

    def popIfFunction(tree: Tree) {
      tree match {
        case _: Function => functionStack.pop
        case _ =>
      }
    }

    def hasFunctionArgument(tree: Tree) = {
      val symbols = tree.collect({case i: Ident => i}).map(_.name).toSet //todo: this shouldn't just be by name
      functionStack.exists(_.vparams.exists(p => symbols(p.name)))
    }


  }

  def injectStructure(tree: Tree, matcher: Tree => Option[Tree]) = {
    val transformer = new Transformer {
      val functionStack = new mutable.Stack[Function]()
      override def transform(tree: Tree) = {
        tree match {
          case f: Function => functionStack.push(f)
          case _ =>
        }
        //        tree match {
        //          case
        //        }
        //todo: replace imports of structure by new temp variable import (or preprocess trees to get rid of imports)
        //        tree match {
        //          case Import(expr,_) => matcher(tree) match {
        //            case Some(structure) =>
        //              val name = newTermName(context.fresh("forImport"))
        //              q"val $name = $structure.value(); im "
        //          }
        //        }
        val result = matcher(tree) match {
          case Some(structure) => {
            //get symbols in tree
            val symbols = tree.collect({case i: Ident => i}).map(_.name).toSet //todo: this shouldn't just be by name
            val hasFunctionArg = functionStack.exists(_.vparams.exists(p => symbols(p.name)))
            if (hasFunctionArg)
              super.transform(tree)
            else
              q"$structure.value()"
          }
          case _ => super.transform(tree)
        }
        tree match {
          case _: Function => functionStack.pop
          case _ =>
        }
        result
      }
    }
    transformer transform tree
  }

  def loopSettings(args: List[Tree])(block: Tree): Tree = args match {
    case Nil => block
    case head :: tail =>
      val inner = loopSettings(tail)(block)
      q"{ $head.resetSetting();  while ($head.hasNextSetting) {  $head.nextSetting(); $inner } }"
  }


}

trait Structure[+T] {
  def nodes(): Iterator[MPGraph.Node]
  def value(): T
  def setToArgmax()
  def resetSetting()
  def hasNextSetting: Boolean
  def nextSetting()
}

trait Observable[T] {
  def observe(t: T)
}

trait HasSettingIterator[T] extends Structure[T] {
  private var iterator: Iterator[Unit] = _
  def newIterator(): Iterator[Unit]
  def hasNextSetting = iterator.hasNext
  def nextSetting() = iterator.next()
  override def resetSetting() = { iterator = newIterator }
}

trait FactorTree {
  def factors: Iterator[Factor]
}

object Structure {
  def loopSettings(structures: List[Structure[Any]])(loop: () => Unit): () => Unit = structures match {
    case Nil => loop
    case head :: tail =>
      def newLoop() {
        head.resetSetting()
        while (head.hasNextSetting) {
          head.nextSetting()
          loop()
        }
      }
      loopSettings(tail)(newLoop)
  }

  def settingsIterator(structures: List[Structure[Any]],
                       iterator: () => Iterator[Unit] = () => Iterator.empty): () => Iterator[Unit] = structures match {
    case Nil => iterator
    case head :: tail =>
      def newIterator() = new Iterator[Unit] {
        head.resetSetting()
        var inner = iterator()
        if (inner.hasNext) {
          head.nextSetting() //this may not work if head has empty domain
        }
        override def next() = {
          if (inner.hasNext) inner.next()
          else {
            head.nextSetting()
            inner = iterator()
            if (inner.hasNext) inner.next()
          }
        }
        override def hasNext = inner.hasNext || head.hasNextSetting
      }
      settingsIterator(tail, newIterator)
  }


}
