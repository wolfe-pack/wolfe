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

    lazy val fieldsAndTypes  = fields zip fieldStructures
    lazy val argTypeName     = tpe.typeSymbol.name.toTypeName
    lazy val className       = newTypeName(context.fresh(tpe.typeSymbol.name.encoded + "Structure"))
    lazy val structureFields = for ((f, t) <- fields zip fieldStructures) yield
      q"val ${newTermName(f.name.encoded)} = new ${t.className}"
    lazy val fieldIds        = fields.map(f => Ident(f.name))
    lazy val fieldValues     = fieldIds.map(i => q"$i.value()")
    lazy val observeFields   = fields.map(f => q"${f.name}.observe(value.${f.name})")
    lazy val argType         = tpe.widen
    def classDef(graphName: TermName) = q"""
      final class $className extends Structure[$argTypeName] {
        import ml.wolfe.MPGraph._
        ..${subClassDefs(graphName)}
        ..$structureFields
        private var iterator:Iterator[Unit] = _
        def fields:Iterator[Structure[_]] = Iterator(..$fieldIds)
        def value():$argTypeName = new $argTypeName(..$fieldValues)
        def nodes():Iterator[Node] = fields.flatMap(_.nodes())
        def resetSetting() { iterator = Structure.settingsIterator(List(..$fieldIds).reverse)()}
        def hasNextSetting = iterator.hasNext
        def nextSetting = iterator.next
        def setToArgmax() {fields.foreach(_.setToArgmax())}
        def observe(value:$argTypeName) { ..$observeFields }
      }
    """
    def children = fieldStructures
    def domainDefs = Nil

    def matcher(parent: Tree => Option[Tree], result: Tree => Option[Tree]): Tree => Option[Tree] = {
      def matchField(field: Symbol)(tree: Tree): Option[Tree] = tree match {
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
}

