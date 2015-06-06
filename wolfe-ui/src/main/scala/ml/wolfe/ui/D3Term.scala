package ml.wolfe.ui

import java.util.UUID

import ml.wolfe.term._
import org.sameersingh.htmlgen.{RawHTML, HTML}

/**
 * @author riedel
 */
object D3Term {

  implicit def render(term:AnyTerm):HTML = {
    val id = "term" + UUID.randomUUID().toString

    def nodeName(term:AnyTerm):String = {
      term match {
        case c:Constant[_] => c.value.toString
        case v:AnyVar => v.varName
        case t => t.getClass.getSimpleName
      }
    }

    def treeData(term:AnyTerm, parent:AnyTerm = null):String = {
      term match {
        case p:ProxyTerm[_] => treeData(p.self,parent)
        case _ =>
          val name = nodeName(term)
          val parentName = if (parent != null) nodeName(parent) else null
          val children = term match {
            case n:NAry => n.arguments map (treeData(_,term))
            case _ => Nil
          }
          val childrenString = children.mkString(",\n")
          val data =
            s"""
               |{
               |  "name": "$name",
               |  "parent": "$parentName",
               |  "children": [$childrenString]
               |}
             """.stripMargin
          data
      }
    }

    def depth(term:AnyTerm):Int = term match {
      case p:ProxyTerm[_] => depth(p.self)
      case n:NAry => (n.arguments map depth).max + 1
      case _ => 1
    }

    val termAsData = treeData(term)
    val termDepth = depth(term)

    val html = s"""
      |<div id = "$id" class="term">
      |<svg></svg>
      |</div>
      |<script>
      |
      |var treeData = [
      |  $termAsData
      |];
      |
      |var depth = $termDepth + 1
      |
      |// ************** Generate the tree diagram	 *****************
      |var margin = {top: 40, right: 120, bottom: 20, left: 120},
      |	width = 960 - margin.right - margin.left,
      |	height = depth * 70 - margin.top - margin.bottom;
      |
      |var i = 0;
      |
      |var tree = d3.layout.tree()
      |	.size([height, width]);
      |
      |var diagonal = d3.svg.diagonal()
      |	.projection(function(d) { return [d.x, d.y]; });
      |
      |var svg = d3.select("#$id svg")
      |	.attr("width", width + margin.right + margin.left)
      |	.attr("height", height + margin.top + margin.bottom)
      |  .append("g")
      |	.attr("transform", "translate(" + margin.left + "," + margin.top + ")");
      |
      |root = treeData[0];
      |
      |update(root);
      |
      |function update(source) {
      |
      |  // Compute the new tree layout.
      |  var nodes = tree.nodes(source).reverse(),
      |	 links = tree.links(nodes);
      |
      |  // Normalize for fixed-depth.
      |  nodes.forEach(function(d) { d.y = d.depth * 50; });
      |
      |  // Declare the nodes…
      |  var node = svg.selectAll("g.node")
      |	  .data(nodes, function(d) { return d.id || (d.id = ++i); });
      |
      |  // Enter the nodes.
      |  var nodeEnter = node.enter().append("g")
      |	  .attr("class", "node")
      |	  .attr("transform", function(d) {
      |		  return "translate(" + d.x + "," + d.y + ")"; });
      |
      |  nodeEnter.append("circle")
      |	  .attr("r", 10);
      |
      |  nodeEnter.append("text")
      |	  .attr("y", function(d) {
      |		  return d.children || d._children ? -18 : 18; })
      |	  .attr("dy", ".35em")
      |	  .attr("text-anchor", "middle")
      |	  .text(function(d) { return d.name; })
      |	  .style("fill-opacity", 1);
      |
      |  // Declare the links…
      |  var link = svg.selectAll("path.link")
      |	  .data(links, function(d) { return d.target.id; });
      |
      |  // Enter the links.
      |  link.enter().insert("path", "g")
      |	  .attr("class", "link")
      |	  .attr("d", diagonal);
      |
      |}
      |
      |</script>
    """.stripMargin
    RawHTML(html)
  }

}
