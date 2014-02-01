package scalapplcodefest.macros

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.collection.mutable
import scalapplcodefest.{MPGraph, MaxProduct}

//import scalapplcodefest.Wolfe._

/**
 * @author Sebastian Riedel
 */
object OptimizedWolfe extends WolfeAPI {

  override def argmax[T, N](data: Iterable[T])
                           (where: (T) => Boolean)
                           (obj: (T) => N)
                           (implicit num: Ordering[N]) = macro implArgmax[T, N]

  def all[A, B](mapper: A => B)(implicit dom: Set[A]): Set[B] = macro implAll[A, B]

  def implAll[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(mapper: c.Expr[A => B])(dom: c.Expr[Set[A]]) = {
    import c.universe._
    DomainExpansions.register(c.Expr(c.macroApplication), mapper, dom)
   reify(dom.splice map mapper.splice)
  }

  def implArgmax[T: c.WeakTypeTag, N: c.WeakTypeTag](c: Context)
                                                    (data: c.Expr[Iterable[T]])
                                                    (where: c.Expr[T => Boolean])
                                                    (obj: c.Expr[T => N])
                                                    (num: c.Expr[Ordering[N]]) = {

    import c.universe._

    val helper = new MacroHelper[c.type](c)

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
    val metaData = helper.Metadata(classes)

    //meta information about the root structure class.
    val root = helper.createStructureType(metaData, rootDom)

    //all structure class definitions
    val classDefs = root.allTypes.map(_.classDef)


    import metaData._

    val optimized = q"""
      ..$imports
      ..$init
      ..${root.domainDefs}
      ..$classDefs
      val $rootName = new ${root.className}
      $rootName.value()
    """

    rootDom match {
      case q"${_}($unwrap[..$types]($constructor))($dom)" =>
        val t: c.Tree = constructor.asInstanceOf[c.Tree]
        val name: TypeName = t.tpe.typeSymbol.name.toTypeName
        val caseClass = classes(name)
        val q"case class $className(..$fields)" = caseClass
        val newFields = fields.collect({
          case q"$mods val $fieldName: $fieldType = ${_}" => q"$mods val $fieldName:Any = _ "
        })
        val newName = newTypeName(className.encoded + "Structure")
        val structureClass = q"""case class $newName(..$newFields) { def test = 1.0} """
        println(newFields)
        println(fields)

        List(unwrap, types, constructor, dom) foreach println
      case _ =>
    }

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
    val rootName      = newTermName(context.fresh("root"))
    val nodeCountName = newTermName(context.fresh("nodeCount"))
    val nextNodeIndex = newTermName(context.fresh("nextNodeIndex"))
    val init          = List(
      q"var $nodeCountName = 0",
      q"def $nextNodeIndex() = {val current = $nodeCountName; $nodeCountName += 1; current}")

    val imports = reify({
      import scalapplcodefest._
      import scalapplcodefest.MPGraph._
      import scalapplcodefest.macros._
    }).tree match {
      case Block(i, _) => i
    }
  }

  def createStructureType(metadata: Metadata, domain: Tree) = {
    domain match {
      case _ => AtomicStructureType(domain, metadata)
    }
  }

  trait StructureType {
    def className: TypeName
    def domainDefs: List[ValDef]
    def classDef: ClassDef
    def children: List[StructureType]
    def allTypes: List[StructureType]
    def value(selector: Tree) = q"$selector.value()"
    def nodes(selector: Tree) = q"$selector.nodes()"
  }

  case class CaseClassType(tpe: Type, fields: List[ValDef], types: List[StructureType]) {

  }

  case class AtomicStructureType(domain: Tree, meta: Metadata) extends StructureType {
    val domName                      = newTermName(context.fresh("atomDom"))
    val className                    = newTypeName(context.fresh("AtomClass"))
    val TypeRef(_, _, List(argType)) = domain.tpe
    val typeName                     = argType.typeSymbol.name.toTypeName
    val domainDefs                   = List(q"val $domName = $domain.toArray")
    def children = Nil
    def allTypes = List(this)
    val classDef = q"""
      class $className extends Structure[$argType] {
        val node = new Node(${meta.nextNodeIndex}(),$domName.length)
        def value() = $domName(node.value)
        def nodes() = Iterator(node)
        def setting() = Iterator(node.setting)
      }
    """

  }

}

trait Structure[T] {
  def nodes(): Iterator[MPGraph.Node]
  def value(): T
  def setting(): Iterator[Int]
}


