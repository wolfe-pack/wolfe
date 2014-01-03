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
class WolfeCompilerPlugin(val global: Global, t: WolfeTransformer) extends Plugin {

  import global._

  val name = "wolfecompiler"
  val description = "compiles and transforms wolfe code"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with Transform {
    val global: WolfeCompilerPlugin.this.global.type = WolfeCompilerPlugin.this.global
    val runsAfter = List("typer")
    val phaseName = WolfeCompilerPlugin.this.name

    protected def newTransformer(unit: global.CompilationUnit): global.Transformer = new global.Transformer {
      override def transform(tree: global.Tree) = {
        t.transformTree[global.Tree](global, super.transform(tree))
      }
    }
  }

}
