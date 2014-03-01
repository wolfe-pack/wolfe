package scalapplcodefest.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class DivByZero(val global: Global) extends Plugin {
  import global._

  val name = "divbyzero"
  val description = "checks for division by zero"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global: DivByZero.this.global.type = DivByZero.this.global
    val runsAfter = List("refchecks")
    // Using the Scala Compiler 2.8.x the runsAfter should be written as below
    // val runsAfter = List[String]("refchecks");
    val phaseName = DivByZero.this.name
    def newPhase(_prev: Phase) = new DivByZeroPhase(_prev)

    class DivByZeroPhase(prev: Phase) extends StdPhase(prev) {
      override def name = DivByZero.this.name
      def apply(unit: CompilationUnit) {
        for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
              if rcvr.tpe <:< definitions.IntClass.tpe)
        {
          unit.error(tree.pos, "definitely division by zero")
        }
      }
    }
  }
}