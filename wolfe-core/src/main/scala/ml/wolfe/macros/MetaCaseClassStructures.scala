package ml.wolfe.macros

import scala.reflect.macros.Context

trait MetaCaseClassStructures[C<:Context] {
  this: MetaStructures[C] =>

  import context.universe._

  /**
   * @author Sebastian Riedel
   */
  trait MetaCaseClassStructure extends MetaStructure {

    self =>

    def tpe: Type
    def fields: List[Symbol]
    def fieldStructures: List[MetaStructure]
    def subClassDefs(graphName: TermName): List[Tree] = fieldStructures.map(_.classDef(graphName))
    /*def subClassDefs(graphName: TermName, label:String): List[Tree] = fieldsAndTypes.map(s =>
      s._2.classDef(graphName, label + "." + s._1.name.toString)
    )*/


    def edgesType = tq"Unit"


    lazy val fieldsAndTypes  = fields zip fieldStructures
    lazy val className       = newTypeName(context.fresh(tpe.typeSymbol.name.encoded + "Structure"))
    lazy val structureFields = for ((f, t) <- fields zip fieldStructures) yield
      q"val ${newTermName(f.name.encoded)} = new ${t.className}(astLabel + '.' + ${f.name.toString})"
    lazy val fieldIds        = fields.map(f => Ident(f.name))
    lazy val fieldValues     = fieldIds.map(i => q"$i.value()")
    lazy val observeFields   = fields.map(f => q"${f.name.toTermName}.observe(value.${f.name.toTermName})")
    lazy val argType         = tpe.widen
    override def classDef(graphName: TermName) = q"""
      final class $className (override val astLabel:String="") extends ml.wolfe.macros.Structure[$tpe] {
        import ml.wolfe.FactorGraph._
        ..${subClassDefs(graphName)}
        ..$structureFields
        private var iterator:Iterator[Unit] = _
        def children() = fields.map(_.asInstanceOf[ml.wolfe.macros.Structure[Any]])
        def fields:Iterator[ml.wolfe.macros.Structure[_]] = Iterator(..$fieldIds)
        def graph = $graphName
        def value():$argType = new $argType(..$fieldValues)
        def nodes():Iterator[Node] = fields.flatMap(_.nodes())
        def resetSetting() { iterator = ml.wolfe.macros.Structure.settingsIterator(List(..$fieldIds).reverse)()}
        def hasNextSetting = iterator.hasNext
        def nextSetting = iterator.next
        def observe(value:$argType) { ..$observeFields }
        type Edges = Unit
        def createEdges(factor: ml.wolfe.FactorGraph.Factor): Edges = {}

      }
    """
    def children = fieldStructures
    def domainDefs = Nil

    def matcher(parent: Tree => Option[StructurePointer],
                result: Tree => Option[StructurePointer]): Tree => Option[StructurePointer] = {
      def matchField(field: Symbol, child:MetaStructure)(tree: Tree): Option[StructurePointer] = tree match {
        case q"$data.$f" if field.name == f => for (s <- parent(data)) yield StructurePointer(q"${s.structure}.$f",child)
        case _ => None
      }
      val fieldMatchers = (fieldsAndTypes zip children).map({case ((f, t),c) => t.matcher(matchField(f,c), matchField(f,c))})
      def firstMatch(matchers: List[Tree => Option[StructurePointer]]): Tree => Option[StructurePointer] = matchers match {
        case Nil => result
        case head :: tail => (t: Tree) => head(t).orElse(firstMatch(tail)(t))
      }
      firstMatch(fieldMatchers)
    }


  }
}

