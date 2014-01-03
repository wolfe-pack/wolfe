package scalapplcodefest.compiler

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

/**
 * @author sameer
 */
class WolfeCompilerPlugin(val global: Global, transform: WolfeTransformer, val runsAfter: List[String] = List("refchecks")) extends Plugin {

  import global._

  val name = "wolfecompiler"
  val description = "compiles and transforms wolfe code"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global: WolfeCompilerPlugin.this.global.type = WolfeCompilerPlugin.this.global
    //    val runsAfter = List("refchecks")
    // Using the Scala Compiler 2.8.x the runsAfter should be written as below
    // val runsAfter = List[String]("refchecks");
    val phaseName = WolfeCompilerPlugin.this.name
    def newPhase(_prev: Phase) = new WolfePhase(_prev)

    class WolfePhase(prev: Phase) extends StdPhase(prev) {
      override def name = WolfeCompilerPlugin.this.name
      def apply(unit: CompilationUnit) {
        transform.transform(global)(unit)
        /*
        for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
             if rcvr.tpe <:< definitions.IntClass.tpe) 
          {
            unit.error(tree.pos, "definitely division by zero")
          }
        */
      }
    }

  }

}
