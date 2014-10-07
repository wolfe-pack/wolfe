package ml.wolfe.macros

import scala.reflect.macros.Context

trait MetaFunStructures[C <: Context] {
  this: MetaStructures[C] =>

  import context.universe._

  /**
   * Representing sample spaces defined as a list of all functions from a domain to a range.
   * @author Sebastian Riedel
   */
  trait MetaFunStructure extends MetaStructure {

    self =>

    /**
     * @return the type of objects of the key.
     */
    def keyType: Type

    def keyDoms: List[Tree]
    def valueMetaStructure: MetaStructure

    //lazy val anyKeyTypes = keyDoms map (_ => tq"Any")
    //lazy val anyKeyType = if (anyKeyTypes.size == 1) tq"Any" else tq"(..$anyKeyTypes)"
    lazy val anyKeyType = keyType match {
      case TypeRef(t, c, args) if keyType.typeSymbol.name.toString.matches("Tuple[0-9]+") =>
        tq"(..${ List.fill(args.length)(tq"Any") })"
      case _ => tq"Any"
    }

    def edgesType = tq"Map[$anyKeyType,${ valueMetaStructure.edgesType }]"

    lazy val keyDomNames   = List.fill(keyDoms.size)(newTermName(context.fresh("funKeyDom")))
    lazy val keyIndexNames = List.fill(keyDoms.size)(newTermName(context.fresh("funKeyIndex")))
    lazy val tmpNames      = Range(0, keyDoms.size).map(i => newTermName("i" + i)).toList
    lazy val tmpIds        = tmpNames.map(Ident(_))
    lazy val argTypeName   = argType.typeSymbol.name.toTypeName
    lazy val tupleArgs     = for ((i, k) <- tmpNames zip keyDomNames) yield q"$k($i)"
    lazy val invTupleArgs  = for ((i, k) <- tmpNames zip keyIndexNames) yield q"$k($i)"
    lazy val tuple         = if (tupleArgs.size == 1) tupleArgs.head else q"(..$tupleArgs)"
    lazy val keyDomSizes   = keyDomNames.map(k => q"$k.length")
    lazy val className     = newTypeName(context.fresh("FunStructure"))
    lazy val domDefs       = for ((d, n) <- keyDoms zip keyDomNames) yield q"val $n = $d.toArray"
    lazy val indexDefs     = for ((d, i) <- keyDomNames zip keyIndexNames) yield q"val $i = $d.zipWithIndex.toMap"
    lazy val domainDefs    = domDefs ++ indexDefs

    def children = List(valueMetaStructure)

    def matcher(parent: Tree => Option[StructurePointer],
                result: Tree => Option[StructurePointer]): Tree => Option[StructurePointer] = {
      def matchApp(tree: Tree) = {
        def replace(f: Tree, args: List[Tree]) = parent(f) match {
          //        case Apply(f, args) => parent(f) match {
          case Some(parentStructure) =>
            val actualArgs = //args
            //todo: if args contains a single element that is a tuple we need to break up the tuple.
              if (args.size == 1 && keyDoms.size > 1) {
                val arg = args.head
                for ((_, i) <- keyDoms.zipWithIndex) yield q"$arg.${ newTermName("_" + (i + 1)) }"
              } else if (args.size > 1 && keyDoms.size == 1) {
                List(q"(..$args)")
              }
              else args

            val asIndices = for ((a, i) <- actualArgs zip keyIndexNames) yield q"${ parentStructure.structure }.$i($a)"
            val substructure = curriedArguments(asIndices, q"${ parentStructure.structure }.subStructures")
            Some(StructurePointer(substructure, valueMetaStructure))
          case _ => None
        }
        tree match {
          case q"$f.apply($tuple.apply[..${_}](..$args))" if scalaSymbols.TupleCompanions(tuple.symbol) =>
            replace(f, args)
          case q"$f.apply(..$args)" =>
            replace(f, args)
          case _ => None
        }
      }
      valueMetaStructure.matcher(matchApp, (t: Tree) => matchApp(t).orElse(result(t)))
    }


    def substructureIterator(count: Int, result: Tree = q"subStructures.iterator"): Tree = count match {
      case 1 => q"$result"
      case n => substructureIterator(n - 1, q"$result.flatMap(_.iterator)")
    }


    lazy val mappings    = tupleProcessor(keyDomNames, tmpNames, q"$tuple -> ${ curriedArguments(tmpIds) }.value")
    lazy val createEdges = tupleProcessor(keyDomNames, tmpNames, q"$tuple -> ${ curriedArguments(tmpIds) }.createEdges(factor)")


    lazy val observeSubStructure = q"${ curriedArguments(tmpIds) }.observe(value($tuple))"

    override def classDef(graphName: TermName) = {
      val iterator = substructureIterator(keyDoms.size)
      val valueDef = valueMetaStructure.classDef(graphName)
      val keyDomIterators = List.fill(keyDoms.size)(newTermName(context.fresh("i")))
      val keyDomIteratorsTyped = keyDomIterators.map(x => q"$x:Int")
      val indexedKeyComponentNames = keyDomIterators.zipWithIndex.map { case (k, i) => q"keyNames($i)($k)" }
      val indexedKeyName = q"""Seq(..$indexedKeyComponentNames).mkString("(", ",", ")")"""
      q"""
      final class $className (override val astLabel : String = "") extends ml.wolfe.macros.Structure[$argType]{
        $valueDef
        ..$domainDefs
        private var iterator:Iterator[Unit] = _
        val keyNames = Seq(..$keyDoms).map(_.map(_.toString))
        def createNode(..$keyDomIteratorsTyped) = new ${ valueMetaStructure.className } (astLabel + $indexedKeyName)
        val subStructures = Array.tabulate(..$keyDomSizes)(createNode)
        def children() = subStructureIterator().map(_.asInstanceOf[ml.wolfe.macros.Structure[Any]])
        def graph = $graphName
        def subStructureIterator() = $iterator
        def nodes() = subStructureIterator().flatMap(_.nodes())
        def resetSetting() { iterator = ml.wolfe.macros.Structure.settingsIterator(subStructureIterator().toList)()}
        def hasNextSetting = iterator.hasNext
        def nextSetting = iterator.next
        def value() = $mappings.toMap
        def observe(value:$argType) {
          ${ tupleProcessor(keyDomNames, tmpNames, observeSubStructure, newTermName("foreach"), newTermName("foreach")) }
        }
        type Edges = $edgesType
        def createEdges(factor: ml.wolfe.FactorGraph.Factor): Edges = {
          $createEdges.toMap
        }

      }
    """
    }

    def curriedArguments(indices: List[Tree], result: Tree = q"subStructures"): Tree = indices match {
      case Nil => result
      case i :: tail => curriedArguments(tail, q"$result($i)")
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


  }

}