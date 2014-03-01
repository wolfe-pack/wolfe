package ml.wolfe.legacy.compiler

import scala.tools.nsc.Global

/**
 * User: rockt
 * Date: 1/5/14
 * Time: 5:02 PM
 */

object WolfeTreePlayground extends App {
  val Imports =
    """
      |  import math._
      |  import ml.wolfe.Wolfe.Output
      |  import ml.wolfe.Wolfe.Objective
      |  import ml.wolfe.Wolfe.Objective.Wolferine
      |  import ml.wolfe.Wolfe.Objective.SymPy
    """.stripMargin


  val simpleCode = Imports +
    """
      |  @Objective.Differentiable(differentiator = Some(Wolferine))
      |  @Output.LaTeX
      |  def simple(z: Double) = z * z
    """.stripMargin

  val additionalClassPath = List(dirPathOfClass(getClass.getName))
  ASTExplorer.exploreAST(simpleCode, List("typer"), showBrowser = true,
    additionalClassPath = additionalClassPath, transformers = List(new RecompileTransformer))
}

class RecompileTransformer extends WolfeTransformer {
  val substituteName = "substitutedFunction"

  val substitute =
    s"""
      |object RecompileTransformerAST {
      |  def $substituteName(z: Double) = 2 * z
      |}
    """.stripMargin


  def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T) = {
    import global._

    tree match {
      case DefDef(a, b, c, d, e, app @ Apply(Select(Ident(f), method), List(Ident(g))))
      if method.decoded == "*" && f.decoded == g.decoded =>
        val recompile = new StringCompiler(runsAfter = List("typer"), foreignGlobal = Some(global))
        val (g, unit) = recompile.compileCode(substitute)
        println(unit.body)

        val substituteCompiled = findRHS(global, env, unit.body)

        treeCopy.DefDef(tree.asInstanceOf[global.Tree], a, b, c, d, e,
          substituteCompiled.asInstanceOf[global.Tree]
        ).asInstanceOf[T]
      case _ => tree
    }
  }

  private def findRHS[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T): T = {
    import global._

    for (node <- tree) node match {
      case DefDef(_, name, _, _, _, rhs) if name.decoded == substituteName => return rhs.asInstanceOf[T]
      case _ => //ignore
    }

    tree
  }
}

