package scalapplcodefest.macros

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.collection.mutable
import scalapplcodefest.MPGraph
import scala.util.parsing.input.OffsetPosition

//import scalapplcodefest.Wolfe._

/**
 * @author Sebastian Riedel
 */
object OptimizedWolfe extends WolfeAPI {

  override def argmax[T](data: Iterable[T])
                        (where: (T) => Boolean)
                        (obj: (T) => Double) = macro implArgmax[T]

  def all[A, B](mapper: A => B)(implicit dom: Set[A]): Set[B] = macro implAll[A, B]

  def implAll[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(mapper: c.Expr[A => B])(dom: c.Expr[Set[A]]) = {
    import c.universe._
    DomainExpansions.register(c.Expr(c.macroApplication), mapper, dom)
    reify(dom.splice map mapper.splice)
  }

  def implArgmax[T: c.WeakTypeTag](c: Context)
                                  (data: c.Expr[Iterable[T]])
                                  (where: c.Expr[T => Boolean])
                                  (obj: c.Expr[T => Double]) = {

    import c.universe._

    val helper = new MacroHelper[c.type](c)

    import helper._

    val Function(List(objVarDef@ValDef(_, objVarName, _, _)), rootObj) = obj.tree
    val Function(List(predVarDef@ValDef(_, predVarName, _, _)), rootPred) = where.tree

    //mapping from val names to definitions (we use methods with no arguments as vals too)
    val vals = c.enclosingUnit.body.collect({
      case ValDef(_, name, _, rhs) => name -> rhs
      case DefDef(_, name, _, Nil, _, rhs) => name -> rhs
    }).toMap

    //classes in context
    val classes = c.enclosingUnit.body.collect({
      case cd@ClassDef(_, name, _, _) => name -> cd
    }).toMap

    //inline the data tree
    val inlinedData = helper.transform(data.tree, {
      case i@Ident(name) => vals.getOrElse(name, i)
      case t => t
    })

    //the domain/sample space. May expand the domain if implicits were involved.
    val rootDom = DomainExpansions.findByPosition(inlinedData.pos.startOrPoint, inlinedData.pos.source) match {
      case Some(expansion) => expansion.term.tree.asInstanceOf[Tree]
      case other => inlinedData
    }


    //information from the enclosing context
    val metaData = Metadata(classes)

    //meta information about the root structure class.
    val root = createStructureType(metaData, rootDom)

    //this builds the factors
    val objFactorsSetup = createFactors(metaData, rootObj, createRootMatcher(objVarName, Ident(metaData.rootName)))

    //turn the predicate into an objective
    val predObj = q"log(I($rootPred))"

    //setting up the factors from the predicate
    val predFactorsSetup = createFactors(metaData, predObj, createRootMatcher(predVarName, Ident(metaData.rootName)))

    //all structure class definitions
    val classDefs = root.allTypes.map(_.classDef)

    import metaData._

    val optimized: Tree = q"""
      ..$imports
      ..$init
      ..${root.allDomainDefs}
      ..$classDefs
      val $rootName = new ${root.className}
      $mpGraphName.setupNodes()
      $objFactorsSetup
      $predFactorsSetup
      $mpGraphName.build()
      MaxProduct($mpGraphName,1)
      $rootName.setToArgmax()
      $rootName.value()
    """

    c.Expr[T](optimized)

  }
}

object DomainExpansions {

  import scala.reflect.internal.util.SourceFile

  case class DomainExpansion(term: Context#Expr[Any], constructor: Context#Expr[Any], domain: Context#Expr[Any])

  case class Location(start: Int, source: SourceFile)

  val byExpression = new mutable.HashMap[Context#Expr[Any], DomainExpansion]()
  val byPosition   = new mutable.HashMap[Location, DomainExpansion]()

  def register(term: Context#Expr[Any], constructor: Context#Expr[Any], domain: Context#Expr[Any]) = {
    val expansion = DomainExpansion(term, constructor, domain)
    byExpression(constructor) = expansion
    byPosition(Location(term.tree.pos.startOrPoint, term.tree.pos.source)) = expansion
  }

  def findByPosition(start: Int, source: SourceFile) = byPosition.get(Location(start, source))

}

class MacroHelper[C <: Context](val context: C) extends TransformHelper[C] with StructureHelper[C] {
}

trait TransformHelper[C <: Context] {
  this: MacroHelper[C] =>

  import context.universe._

  def transform(tree: Tree, pf: PartialFunction[Tree, Tree]): context.Tree = new TransformWithPartialFunction(pf).transform(tree)

  class TransformWithPartialFunction(pf: PartialFunction[Tree, Tree]) extends Transformer {
    override def transform(tree: Tree) = {
      val transformed = super.transform(tree)
      if (pf.isDefinedAt(transformed)) pf(transformed) else transformed
    }
  }
}

trait StructureHelper[C <: Context] {
  this: MacroHelper[C] =>

  import context.universe._


  case class Metadata(classes: Map[TypeName, ClassDef]) {
    val mpGraphName   = newTermName(context.fresh("mpGraph"))
    val rootName      = newTermName(context.fresh("root"))
    val nodeCountName = newTermName(context.fresh("nodeCount"))
    val nextNodeIndex = newTermName(context.fresh("nextNodeIndex"))
    val init          = List(q"val $mpGraphName = new MPGraph()")

    val imports = reify({
      import scalapplcodefest._
      import scalapplcodefest.Wolfe._
      import scalapplcodefest.MPGraph._
      import scalapplcodefest.macros._
    }).tree match {
      case Block(i, _) => i
    }
  }

  def createStructureType(metadata: Metadata, domain: Tree): StructureType = {
    domain match {
      case q"$all[..${_}]($unwrap[..${_}]($constructor))($cross(..$sets))"
        if all.symbol.name.encoded == "all" && unwrap.symbol.name.encoded.startsWith("unwrap") =>
        val tpe = constructor.tpe
        val caseClassName = tpe.typeSymbol.name.toTypeName
        val caseClass = metadata.classes(caseClassName)
        val q"case class $className(..$fields)" = caseClass
        val subtypes = sets.map(createStructureType(metadata, _))
        CaseClassType(tpe, fields, subtypes)
      case _ => AtomicStructureType(domain, metadata)
    }
  }

  trait StructureType {
    def className: TypeName
    def domainDefs: List[ValDef]
    def allDomainDefs = allTypes.flatMap(_.domainDefs)
    def classDef: ClassDef
    def argType: Type
    def children: List[StructureType]
    def allTypes: List[StructureType]
    def value(selector: Tree) = q"$selector.value()"
    def nodes(selector: Tree) = q"$selector.nodes()"
  }

  case class CaseClassType(tpe: Type, fields: List[ValDef], types: List[StructureType]) extends StructureType {
    val argTypeName     = tpe.typeSymbol.name.toTypeName
    val className       = newTypeName(context.fresh(tpe.typeSymbol.name.encoded + "Structure"))
    val structureFields = for ((ValDef(mods, name, _, _), t) <- fields zip types) yield
      q"val $name = new ${t.className}"
    val fieldIds        = fields.map(f => Ident(f.name))
    val fieldValues     = fieldIds.map(i => q"$i.value()")
    val classDef        = q"""
      class $className extends Structure[$argTypeName] {
        ..$structureFields
        private var iterator:Iterator[Unit] = _
        def fields:Iterator[Structure[Any]] = Iterator(..$fieldIds)
        def value():$argTypeName = new $argTypeName(..$fieldValues)
        def nodes():Iterator[Node] = fields.flatMap(_.nodes())
        def resetSetting() { iterator = Structure.settingsIterator(List(..$fieldIds).reverse)()}
        def hasNextSetting = iterator.hasNext
        def nextSetting = iterator.next
        def setToArgmax() {fields.foreach(_.setToArgmax())}
      }
    """

    def allTypes = this :: types.flatMap(_.allTypes)
    def children = types
    def argType = tpe
    def domainDefs = Nil


  }

  case class AtomicStructureType(domain: Tree, meta: Metadata) extends StructureType {
    val domName                      = newTermName(context.fresh("atomDom"))
    val className                    = newTypeName(context.fresh("AtomicStructure"))
    val TypeRef(_, _, List(argType)) = domain.tpe
    val argTypeName                  = argType.typeSymbol.name.toTypeName
    val domainDefs                   = List(q"val $domName = $domain.toArray")
    def children = Nil
    def allTypes = List(this)
    val classDef = q"""
      class $className extends Structure[$argType] {
        val node = ${meta.mpGraphName}.addNode($domName.length)
        private def updateValue() {node.value = node.domain(node.setting)}
        def value() = $domName(node.value)
        def nodes() = Iterator(node)
        def resetSetting() {node.setting = -1}
        def hasNextSetting = node.setting < node.dim - 1
        def nextSetting() = {node.setting += 1; updateValue()}
        def setToArgmax() { node.setting = MoreArrayOps.maxIndex(node.b); updateValue()}
      }
    """

  }

  def createRootMatcher(name: TermName, rootStructure: Tree)(tree: Tree): Option[Tree] = tree match {
    //    case id: Ident if id.tpe =:= typ => Some(rootStructure)
    case Ident(n) if n == name => Some(rootStructure)
    case _ => None
  }


  def createFactors(metadata: Metadata, potential: Tree, matchStructure: Tree => Option[Tree]): Tree = potential match {
    case _ =>
      //get sub-structures using matcher
      val arguments = structures(potential, matchStructure)

      val nodesPerArg = arguments.map(a => q"$a.nodes()")

      val nodes = q"""Iterator(..$nodesPerArg).flatMap(identity)"""

      //the potential may be partly typed and untyped. This creates problems and hence we remove all type information here
      val cleaned = context.resetAllAttrs(potential)

      //transform potential to use structure values
      val injected = injectStructure(cleaned, matchStructure)

      //val typed = context.typeCheck(injected)
      //iterate over settings and values

      val perSetting = q"""
        println(nodes.map(_.setting).mkString(","))
        settings(settingIndex) = nodes.map(_.setting)
        scores(settingIndex) = $injected
        settingIndex += 1
      """

      val loop = loopSettings(arguments) {perSetting}

      val setup = q"""
      {
        val nodes = $nodes.toArray
        val dims = nodes.map(_.dim)
        val settingsCount = dims.product
        val settings = Array.ofDim[Array[Int]](settingsCount)
        val scores = Array.ofDim[Double](settingsCount)
        var settingIndex = 0
        $loop
        val factor = ${metadata.mpGraphName}.addTableFactor(scores, settings, dims)
        nodes.view.zipWithIndex.foreach(p => ${metadata.mpGraphName}.addEdge(factor,p._1,p._2))
      }
      """

      setup
  }

  def structures(tree: Tree, matchStructure: Tree => Option[Tree]): List[Tree] = {
    var result: List[Tree] = Nil
    val traverser = new Traverser {
      override def traverse(tree: Tree) = {
        matchStructure(tree) match {
          case Some(structure) =>
            result ::= structure
          case _ =>
            super.traverse(tree)
        }
      }
    }
    traverser traverse tree
    result
  }

  def injectStructure(tree: Tree, matcher: Tree => Option[Tree]) = {
    val transformer = new Transformer {
      override def transform(tree: Tree) = {
        matcher(tree) match {
          case Some(structure) => q"$structure.value()"
          case _ => super.transform(tree)
        }
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

