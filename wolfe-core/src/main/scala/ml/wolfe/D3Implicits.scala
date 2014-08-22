package ml.wolfe

import java.io.{File, PrintWriter}

import ml.wolfe.FactorGraph.{Factor, Node}
import ml.wolfe.fg.{TupleVar, DiscreteVar}
import org.sameersingh.htmlgen.{RawHTML, HTML}
import scala.language.implicitConversions

/**
 * Created by luke on 20/08/14.
 */
object D3Implicits {

  private def escape(s:String) =
    s.replace("\n","\\n").replace("'", "\\'").replace("\"", "\\\"")

  private def wrapCode(id:String, javascript:String, style:String = "") : HTML = {
    val html =
      s"""
        |	<div id="$id">
        |		<style type="text/css">
        |			$style
        |		</style>
        |
        |		<script type="text/javascript">
        |     var onD3Loaded = function() {
        |			  $javascript
        |     }
        |
        |			if(typeof d3 == "undefined") {
        |				var head= document.getElementsByTagName('head')[0];
        |				var script= document.createElement('script');
        |				script.type= 'text/javascript';
        |				script.src= 'http://d3js.org/d3.v3.min.js';
        |				script.onload = onD3Loaded;
        |				head.appendChild(script);
        |			} else {
        |				onD3Loaded();
        |			}
        |		</script>
        |	</div>
      """.stripMargin

    RawHTML(html)
  }



  // ----- Implicits -------------------------------------------------------------------
  def barChart(v:Wolfe.Vector) : HTML = {
    val id = "VEC" + Math.abs(v.hashCode()).toString
    val dataid = id + "_data"
    val nonZero = v.filter(_._2 != 0)

    val width = 500

    val barHeight = 30
    val barSpace = 10

    val offset = 100

    val data = s"var data = { ${nonZero.map(x => "\"" + escape(x._1.toString) + "\": " + x._2).mkString(", ")} };"
    val run =
      s"""
|				var barHeight = $barHeight;
|				var barSpace = $barSpace;
|
|				svg = d3.select("#$id").append("svg")
|       .attr("height", d3.entries(data).length * (barHeight + barSpace))
|				.attr("width", $width)
|       .attr("class", "barchart")
|
|				var scale = d3.scale.linear()
|				.domain([0, d3.max(d3.values(data))])
|				.range([0, $width - $offset]);
|
|				var bar = svg.selectAll("g")
|				.data(d3.entries(data))
|				.enter().append("g")
|				.attr("transform", function(d, i) { return "translate($offset," + i * (barHeight + barSpace) + ")"; });
|
|				bar.append("rect")
|				.attr("width", function(d) { return scale(d.value); })
|				.attr("height", barHeight)
|				.attr("class", "bar")
|
|				bar.append("text")
|				.attr("x", -15)
|				.attr("y", barHeight / 2)
|       .attr("dy", ".35em")
|       .attr("text-anchor", "end")
|       .attr("class", "barlabel")
|				.text(function(d) { return d.key; });
|
|				bar.append("text")
        .attr("text-anchor", function(d) {return scale(d.value)>10 ? "end" : "start";})
|				.attr("x", function(d) {return scale(d.value)>10 ? scale(d.value)-4 : 4;})
|				.attr("y", barHeight / 2)
|       .attr("dy", ".35em")
|       .attr("class", "barvalue")
|				.text(function(d) { return d.value; });
      """.stripMargin

    wrapCode(id, data + run)
  }

  implicit def d3fg(fg:FactorGraph) = $d3fg(fg)
  private def $d3fg(fg:FactorGraph, width:Int=600, height:Int=200, nodeFilter:Node=>Boolean = _=>true, linear:Boolean=true):HTML = {
    val id = "FG" + Math.abs(fg.hashCode()).toString

    def nodeToNumber(n:Node) =  """(.*)\(([0-9]+)\)""".r.findFirstMatchIn(n.variable.label) match {
      case Some(x) => x.group(2).toInt
      case None => -1
    }
    def nodeToType(n:Node) = """(.*)\(([0-9]+)\)(.*)""".r.findFirstMatchIn(n.variable.label) match {
      case Some(x) => x.group(3)
      case None => ""
    }
    

    val nodes = fg.nodes.filter(nodeFilter)
    val factors = fg.factors.filter(_.edges.map(_.n).forall(nodeFilter))
    val edges = fg.edges.filter(e => nodes.contains(e.n) && factors.contains(e.f))

    val maxNodeNumber = nodes.map(nodeToNumber).max
    val minNodeNumber = if(maxNodeNumber > -1) nodes.map(nodeToNumber).filter(_ > -1).min else -1
    val nodeTypes = nodes.map(nodeToType).distinct

    //val width = 620
    // val height = 300

    val gravity = 0.03
    val charge = -150
    val linkDistance = 50
    val linkStrength = 0.5

    def nodeX(n:Node) = width * (
      if(nodeToNumber(n) == -1) Math.random()
      else ((nodeToNumber(n)-minNodeNumber).toDouble+0.5)/(maxNodeNumber - minNodeNumber +1))
    def nodeY(n:Node) = height * (
      if(nodeToNumber(n) == -1) Math.random()
      else (nodeTypes.indexOf(nodeToType(n))+1).toDouble / (nodeTypes.length+1))
    def factorX(f:Factor) = f.edges.map(e => nodeX(e.n)).sum / f.edges.length
    def factorY(f:Factor) = f.edges.map(e => nodeY(e.n)).sum / f.edges.length
    def isFixed(n:Node) = linear && maxNodeNumber != -1 && (nodeToNumber(n) == minNodeNumber || nodeToNumber(n) == maxNodeNumber)



    val style =
      s"""
        |.link {
        |	stroke: #000;
        |	stroke-width: 1.5px;
        |}
        |
        |.fgshape {
        |	cursor: move;
        |}
        |
        |.label {
        |	pointer-events:none;
        |	-moz-user-select: -moz-none;
        |	-khtml-user-select: none;
        |	-webkit-user-select: none;
        |	-o-user-select: none;
        |	user-select: none;
        |}
        |
        |.tooltip {
        |	padding:0px;
        |	pointer-events:none;
        |	-moz-user-select: -moz-none;
        |	-khtml-user-select: none;
        |	-webkit-user-select: none;
        |	-o-user-select: none;
        |	user-select: none;
        |}
        |
        |.tooltipinner {
        |	pointer-events:none;
        |	overflow:hidden;
        |}
""".stripMargin

    val data =  s"""
        |var data = {graph:{
        |  "nodes": [${(
              nodes.map(n =>
                "{text:'" + escape(n.variable.label) + "'" +
                ", type:'node'" +
                 ", hoverhtml:'Domain:<br/>{" + escape(n.variable match {
                  case v:DiscreteVar => v.domainLabels.mkString(", ")
                  case v:TupleVar => v.domainLabels.mkString(", ")
                  case _ => ""
                }) + "}'" +
                ", x:" + nodeX(n) + ", y:" + nodeY(n) +
                ", fixed:" + isFixed(n) +
                "}") ++
              factors.map(f =>
                "{type:'factor'" +
                ", hoverhtml:'" + "<div class=\"tooltipheader\">" + escape(f.label) + "</div>" +
                escape(f.potential.toHTMLString(FactorGraph.DefaultPrinter)) + "'" +
                ", x:" + factorX(f) + ", y:" + factorY(f) +
                "}")
              ).mkString(", ")}
        |  ],
        |  "links": [
        |    ${edges.map(e =>
      "{'source': " + nodes.indexOf(e.n) + ", 'target': " + (factors.indexOf(e.f) + nodes.length) + "}"
    ) mkString ", "}
        |  ]
        |}}
      """.stripMargin

    val run = s"""
        |var force = d3.layout.force()
        |.size([$width, $height])
        |.charge($charge)
        |.gravity($gravity)
        |.linkStrength($linkStrength)
        |.linkDistance($linkDistance)
        |    drag = force.drag()
        |    svg = d3.select("#$id").append("svg")
        |     .attr("class", "factorgraph")
        |     .attr("width", $width)
        |     .attr("height", $height)
        |     .style("overflow", "visible");
        |    link = svg.selectAll(".link")
        |    node = svg.selectAll(".fgshape")
        |    label = svg.selectAll(".label");
        |
        |    force
        |     .nodes(data.graph.nodes)
        |     .links(data.graph.links)
        |     .start();
        |
        |    var link = link.data(data.graph.links)
        |	    .enter().append("line")
        |	    .attr("class", "link");
        |
        |	  var node = node.data(data.graph.nodes)
        |	    .enter().append("path")
        |	    .attr("class", function(d) {
        |       return 'fgshape' +
        |         (d.type == 'factor' ? ' fgfactor' : ' fgnode') +
        |         (d.fixed ? ' fgfixed' : '');
        |     })
        |	    .attr("d", d3.svg.symbol()
        |	    	.type(function(d) { return d.type == 'factor' ? 'square' : 'circle' })
        |	    	.size(function(d) { return d.type == 'factor' ? 500 : 2000 }))
        |	    .on("mouseover", function(d){
        |	    	if(d.hoverhtml != undefined) {
        |	    		setTooltip(d.hoverhtml);
        |	    		tooltip.transition()
        |	    			.duration(300)
        |	    			.style("opacity", .9);
        |	    		tooltipNode = d;
        |	    		moveTooltip();
        |	    	}
        |	    })
        |	    .on("mouseout", function(d){
        |			tooltip.transition()
        |	                .duration(300)
        |	                .style("opacity", 0)
        |	    })
        |		.call(drag);
        |var label = label.data(data.graph.nodes)
        |	.enter().append("text")
        |	.attr("class", "label")
        |	.attr("text-anchor", "middle")
        |	.text(function(d) { return d.text == undefined ? "" : d.text })
        | .style("font-size", function(d) { return Math.min(700 / this.getComputedTextLength(), 15) + "px"; })
        |	.attr("dy", ".35em")
        |	.call(drag);
        |
        |tooltipNode = null
        |tooltip = null
        |
        |
        |var checkBounds = function() {
        |    node.each(function(d) {
        |     d.x = Math.max(25, Math.min($width-25, d.x));
        |     d.y = Math.max(25, Math.min($height-25, d.y));
        |    });
        |}
        |
        |force.on("tick", checkBounds);
        |for(var i=0; i<100; i++) {force.alpha(0.1); force.tick();}
        |/*while(force.alpha() > 0.01) {force.tick();}*/
        |
        |var tick = function() {
        | checkBounds();
        |	link.attr("x1", function(d) { return d.source.x; })
        |		.attr("y1", function(d) { return d.source.y; })
        |		.attr("x2", function(d) { return d.target.x; })
        |		.attr("y2", function(d) { return d.target.y; });
        |
        |
        |	node.attr("transform", function(d) {return "translate(" + d.x + "," + d.y + ")"});
        |	label.attr("transform", function(d) {return "translate(" + d.x + "," + (d.y) + ")"});
        |	moveTooltip();
        |
        |}
        |
        |
        |var setTooltip = function(html) {
        |	if(tooltip != null) {
        |		tooltip.remove()
        |	}
        |	tooltip = svg.insert("foreignObject")
        |		.attr("class", "tooltip")
        |		.attr("width", "300")
        |		.attr("height", "500")
        |		.style("opacity", 0)
        |		.html("<div class='tooltipinner'>" + html + "</div>")
        |}
        |
        |
        |var moveTooltip = function() {
        |	if(tooltipNode != null) {
        |		tooltip.attr("transform", "translate(" + (tooltipNode.x-150) + "," + (tooltipNode.y+15) + ")" );
        |	}
        |}
        |
        |force.on("tick", tick);
        |tick();
      """.stripMargin

    wrapCode(id, data + run, style)
  }




  // -- Save to local file -----------------------------------------------------------------------

  def defaultLocation = System.getProperty("user.home") + "/wolfe_d3"

  def saveD3Graph(fg:FactorGraph,
               file:String = defaultLocation + "/factorgraph.html",
               regexFilter:String = ".*") = {
    val html = $d3fg(fg, 1200, 800, _.variable.label.matches(regexFilter), true)
    save(html, file)
  }

  def saveD3BarChart(v:Wolfe.Vector,
             file:String = defaultLocation + "/vector.html") = {
    val html = barChart(v)
    save(html, file)
  }

  private def save(html:HTML, file:String) = {
    if(file.startsWith(defaultLocation) && !new File(defaultLocation).exists())
      new File(defaultLocation).mkdir()

    val writer = new PrintWriter(file)
    writer.println("<html>" +
    "<head><link rel='stylesheet' href='" +
        //"https://raw.githubusercontent.com/wolfe-pack/moro/master/public/stylesheets/wolfe.css" +
          "/home/luke/workspace/moro/public/stylesheets/wolfe.css" +
    "' />" +
    "</head>" +
    "<body>" + html.source + "</body>" +
    "</html>")

    writer.close()
  }
}
