package scalapplcodefest.compiler

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

/**
 * @author sameer
 */
class WolfeCompilerPlugin(val global: Global, transformer: WolfeTransformer, val runsAfter: List[String] = List("refchecks")) extends Plugin {
  self =>

  import global._

  val name = "wolfecompiler"
  val description = "compiles and transforms wolfe code"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with Transform {
    val global: WolfeCompilerPlugin.this.global.type = WolfeCompilerPlugin.this.global
    val runsAfter = self.runsAfter
    val phaseName = WolfeCompilerPlugin.this.name

    protected def newTransformer(unit: global.CompilationUnit): global.Transformer = new global.Transformer {
      override def transform(tree: global.Tree) = {
        transformer.transformTree[global.Tree](global, null, super.transform(tree))
      }
    }
  }

}
