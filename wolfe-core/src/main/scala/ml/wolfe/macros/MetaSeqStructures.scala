package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait MetaSeqStructures[C<:Context] {
  this:MetaStructures[C] =>

  import context.universe._

  trait MetaSeqStructure extends MetaStructure {
    def elementMetaStructure:MetaStructure
    def lengthInitializer:Tree

    var lengthInitialized = lengthInitializer != EmptyTree


    def edgesType = tq"Seq[${elementMetaStructure.edgesType}]"

    val test:Tree = tq"Unit"

    lazy val className = newTypeName(context.fresh("SeqStructure"))
    override def classDef(graphName:TermName) = {
      val elementDef = elementMetaStructure.classDef(graphName)
      val elementEdgesType = stringToTypeName(className.toString  + ".this." + elementMetaStructure.className.toString + "#Edges")

      q"""
        final class $className (override val astLabel:String="") extends ml.wolfe.macros.Structure[$argType] with ml.wolfe.macros.SeqStructure[$argType] {

          $elementDef

          private var iterator:Iterator[Unit] = _

          private var _elements:Array[${elementMetaStructure.className}] = null
          def setLength(length:Int) {
            _elements = (0 until length).map({x:Int =>
              new ${elementMetaStructure.className} (astLabel + "(" + x.toString + ")")
            }).toArray
          }
          def elements = _elements
          def children() = elements.iterator.map(_.asInstanceOf[ml.wolfe.macros.Structure[Any]])
          def nodes = _elements.iterator.flatMap(_.nodes)
          def graph = $graphName

          def resetSetting() { iterator = ml.wolfe.macros.Structure.settingsIterator(_elements.toList)()}
          def hasNextSetting = iterator.hasNext
          def nextSetting = iterator.next

          def value() = _elements.view.map(_.value()).toList

          def observe(value:$argType) {
            if (_elements == null || _elements.length != value.size) setLength(value.size)
            value.indices.foreach { (i:Int) =>  _elements(i).observe(value(i)) }
          }
          type Edges = $edgesType
          def createEdges(factor: ml.wolfe.FactorGraph.Factor): Edges = {
            (_elements map (_.createEdges(factor))).toSeq
          }


          $lengthInitializer
        }
        """
    }
    def children = List(elementMetaStructure)
    def domainDefs = Nil
    /**
     * A matcher takes a tree and returns a tree that represents the sub-structure that corresponds to the
     * tree, if any. This method creates matchers for the structures of this meta-structure.
     * @param parent a matcher for the parent structure.
     * @param result the current result matcher.
     * @return a matcher...
     */
    def matcher(parent: Tree => Option[StructurePointer], result: Tree => Option[StructurePointer]) = {
      def matchElementOf(tree:Tree):Option[StructurePointer] = tree match {
        case q"$f.apply($arg)" => parent(f) match {
          case Some(pointer) =>
            Some(StructurePointer(q"${pointer.structure}.elements($arg)",elementMetaStructure))
          case _ =>
            None
        }
        case _ => None
      }
      elementMetaStructure.matcher(matchElementOf, (t: Tree) => matchElementOf(t).orElse(result(t)))
    }
  }

}

/**
 * Structures that represent fixed length sequences.
 */
trait SeqStructure[T] extends Structure[T] {
  def setLength(length:Int)
}
