package scalapplcodefest.compiler

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{PluginComponent, Plugin}
import scala.tools.nsc.transform.Transform
import scala.collection.mutable
import scala.reflect.internal.Phase

/**
 * @author sameer
 */
class WolfeCompilerPlugin2(val global: Global, transformer: WolfeTransformer, val runsAfter: List[String] = List("refchecks")) extends Plugin {
  self =>

  import global._

  val name = "wolfecompiler"
  val description = "compiles and transforms wolfe code"
  val components = List[PluginComponent](Collector, Transformer)

  private val implementations = new mutable.HashMap[Symbol,Tree]

  private object Transformer extends PluginComponent with Transform {
    val global: WolfeCompilerPlugin2.this.global.type = WolfeCompilerPlugin2.this.global
    val runsAfter = List(Collector.phaseName)
    val phaseName = WolfeCompilerPlugin2.this.name

    protected def newTransformer(unit: global.CompilationUnit): global.Transformer = new global.Transformer {
      override def transform(tree: global.Tree) = {
        transformer.transformTree[global.Tree](global, implementations.toMap, super.transform(tree))
      }
    }
  }

  private object Collector extends PluginComponent  {
    val global: WolfeCompilerPlugin2.this.global.type = WolfeCompilerPlugin2.this.global
    val runsAfter = self.runsAfter
    val phaseName = "Collection"

    class CollectionPhase(prev:Phase) extends StdPhase(prev) {
      override def name = Collector.phaseName
      def apply(unit: CompilationUnit) = {
        import global._
        unit.body.foreach {
          case d@DefDef(_, _, _, _, _, rhs) =>
            implementations(d.symbol) = rhs
          case d@ValDef(_, _, _, rhs) =>
            implementations(d.symbol) = rhs
          case _ =>
        }
      }
    }

    def newPhase(prev: Phase) = {
      new CollectionPhase(prev)
    }


  }


}
