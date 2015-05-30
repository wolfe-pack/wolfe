package ml.wolfe.ui

import ml.wolfe.term._
import ml.wolfe.term.TermImplicits._
import org.sameersingh.htmlgen.{HTML, RawHTML}
import Json._

/**
 * @author Luke Hewitt
 */
object FactorGraphRenderer {

  val fgScriptLocation = "/assets/javascripts/factorgraph/fg.js"

  def wrapCode(id: String, fgData:String): HTML = {
    val script =
      s"""
        |<script type="text/javascript">
        |        var fgScriptLocation;
        |             if (typeof fgScriptLocation === 'undefined') {
        |                fgScriptLocation = "$fgScriptLocation";
        |                head.js(fgScriptLocation);
        |             }
        |             head.ready(function() {
        |                var fgData = ${indentStr(29, fgData)};
        |                FG.create("$id", fgData);
        |            });
        |    </script>
      """.stripMargin

    val html =
      s"""
        | <div id="$id"></div>
        |
        | <link rel="stylesheet" type="text/css" href="/assets/stylesheets/factorgraph/fg.css"/>
        |$script
      """.stripMargin
    println(html)
    RawHTML(html)
  }


  private def foo (fg:FG[MaxProductBP#NodeContent, MaxProductBP#EdgeContent, MaxProductBP#FactorContent]):HTML = {

    def updateNodeBelief(node: fg.Node): Unit = {}

    val width = 600
    val height = 400
    val id = "FG" + Math.abs(fg.hashCode()).toString

    val gravity = 0.03
    val charge = -200
    val linkDistance = 50
    val linkStrength = 0.5
    val strokeWidth = 2
    def nodeX(n:fg.Node) = width * Math.random()
    def nodeY(n:fg.Node) = width * Math.random()

    for (n <- fg.activeNodes) {
      updateNodeBelief(n)
    }

    def factorY(f:fg.Factor) = f.edges.map(e => nodeY(e.node)).sum / f.edges.length + 20
    def isFixed(n:fg.Node) = false

    val data = fg.nodes.map(n => n.toString())
    RawHTML(data.reduce(_ + "\n" + _))
  }

  def foo[NodeContent, EdgeContent, FactorContent](fg: FG[NodeContent, EdgeContent, FactorContent]) = {

    val nodesSeq = fg.nodes.toList
    val nodesIndex = nodesSeq.map(_._2).zipWithIndex.toMap
    val nodes = nodesSeq.map{case (atom, node) =>
      toJson(Map(
        "type"      -> toJson("node"),
        "text"      -> toJson(atom.toString),
        "hoverhtml" -> toJson("Hello World"),
        "x"         -> toJson(Math.random()*600),
        "y"         -> toJson(Math.random()*400),
        "fixed"     -> toJson(false)
      ))
    }
    val nNodes = fg.nodes.size

    val factorsSeq = fg.factors.toList
    val factorsIndex = factorsSeq.zipWithIndex.map{ case((t, f), i) => (f, i+nNodes)}.toMap
    val factors = factorsSeq.map{case (term, factor) =>
      toJson(Map(
        "type"      -> toJson("factor"),
        "hoverhtml" -> toJson(term.toString)
      ))
    }

    val links = fg.edges.map { e =>
      toJson(Map(
        "target" -> nodesIndex(e.node),
        "source" -> factorsIndex(e.factor)/*,
        "source_" -> e.node.variable.toString,
        "target_" -> e.factor.potential.toString*/
      ))
    }

    val data = toJson(Map(
      "width" -> toJson(650),
      "height" -> toJson(250),
      "graph" -> toJson(Map(
        "nodes" -> (nodes ++ factors),
        "links" -> links
      ))
    ))

    data
  }

  def bar = {
    val n = 5
    val vars = Range(0, n) map (i => Bools.Variable("y" + i))
    val length = Ints(0 until n).Var
    def local(b: BoolTerm) = I(b)
    def pair(b: (BoolTerm, BoolTerm)) = I(b._1 <-> ! b._2)
    val pairs = vars.dropRight(1) zip vars.drop(1)
    val obj = sum(vars.map(local), length) + sum(pairs.map(pair), length - 1)
    val observation = Settings.fromSeq(Seq(Setting.disc(5)))
    val argmaxer = new MaxProductBP(obj, vars, observation, null)(BPParameters(1))
    argmaxer.argmax()(Execution(0))
    val fg = argmaxer.fg


    // -------------------------------------------
    val id = "fg" + Math.abs(fg.hashCode()).toString
    val data = foo(fg)

    wrapCode(id, data.str)
  }

  def main(args:Array[String]): Unit = {
    println(bar)
  }
}
