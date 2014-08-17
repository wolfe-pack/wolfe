package ml.wolfe

import java.io.PrintWriter

import ml.wolfe.FactorGraph.{Factor, Node}
import org.sameersingh.htmlgen.{RawHTML, HTML}

object FactorGraphViewer {

  def nodeToNumber(n:Node) =  """(.*)\(([0-9]+)\)""".r.findFirstMatchIn(n.variable.label) match {
      case Some(x) => x.group(2).toInt
      case None => -1
    }
  def nodeToType(n:Node) = """(.*)\(([0-9]+)\)(.*)""".r.findFirstMatchIn(n.variable.label) match {
    case Some(x) => x.group(3)
    case None => ""
  }

  def saveHTML(fg:FactorGraph,
                      file:String = System.getProperty("user.home") + "/factorgraph.html",
                      regexFilter:String = ".*") = {
    val html = FactorGraphViewer.toD3Html(fg, 1200, 800, _.variable.label.matches(regexFilter), true).source
    val writer = new PrintWriter(file)
    writer.println("<html>" +
    "<head><link rel='stylesheet' href='" +
      //"http://moro.wolfe.ml:9000/assets/stylesheets/wolfe.css" +
      "./workspace/moro/public/stylesheets/wolfe.css" +
    "' />" +
    "<style type='text/css'>.label{fill:#fff;font-size:15px;text-transform:none}</style>" +
    "</head>" +
    "<body>" + html + "</body>" +
    "</html>")

    writer.close()
  }

  def toD3Html(fg:FactorGraph, width:Int=620, height:Int=300, nodeFilter:Node=>Boolean, linear:Boolean=false):HTML = {
    def escape(s:String) =
      s.replace("\n","\\n").replace("'", "\\'").replace("\"", "\\\"")

    val fgid = this.hashCode().toString

    val nodes = fg.nodes.filter(nodeFilter)
    val factors = fg.factors.filter(_.edges.map(_.n).forall(nodeFilter))
    val edges = fg.edges.filter(e => nodes.contains(e.n) && factors.contains(e.f))

    val maxNodeNumber = nodes.map(nodeToNumber).max
    val minNodeNumber = if(maxNodeNumber > -1) nodes.map(nodeToNumber).filter(_ > -1).min else -1
    val nodeTypes = nodes.map(nodeToType).distinct

    //val width = 620
   // val height = 300

    val gravity = 0.03
    val charge = -700
    val linkDistance = 50

    def nodeX(n:Node) = width * (
      if(nodeToNumber(n) == -1) Math.random()
      else ((nodeToNumber(n)-minNodeNumber).toDouble+0.5)/(maxNodeNumber - minNodeNumber +1))
    def nodeY(n:Node) = height * (
      if(nodeToNumber(n) == -1) Math.random()
      else (nodeTypes.indexOf(nodeToType(n))+1).toDouble / (nodeTypes.length+1))
    def factorX(f:Factor) = f.edges.map(e => nodeX(e.n)).sum / f.edges.length
    def factorY(f:Factor) = f.edges.map(e => nodeY(e.n)).sum / f.edges.length
    def isFixed(n:Node) = linear && maxNodeNumber != -1 && (nodeToNumber(n) == minNodeNumber || nodeToNumber(n) == maxNodeNumber)


    val genCode =  s"""
        |var FG$fgid = {graph:{
        |  "nodes": [${(
             nodes.map(n =>
                "{text:'" + escape(n.variable.label) + "'" +
                ", type:'node'" +
                ", hoverhtml:'Domain: {" + escape(n.variable.asDiscrete.domainLabels.mkString(", ")) + "}'" +
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

    val code =
      s"""
        |<div id="FG$fgid">
        |<style type="text/css">
        |
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
        |/*
        |.tooltipbox {
        |	pointer-events:none;
        |}*/
        |
        |.tooltipinner {
        |	pointer-events:none;
        |	overflow:hidden;
        |	max-height:400px;
        |}
        |
        |</style>
        |
        |
        |
        |<script>
        |$genCode
        |</script>
        |
        |<script type="text/javascript">
        |var onD3Loaded = function() {
        |FG$fgid.width = $width,
        |FG$fgid.height = $height;
        |
        |FG$fgid.force = d3.layout.force()
        |.size([FG$fgid.width, FG$fgid.height])
        |.charge($charge)
        |.gravity($gravity)
        |.linkDistance($linkDistance)
        |
        |    FG$fgid.drag = FG$fgid.force.drag()
        |
        |    FG$fgid.svg = d3.select("#FG$fgid").append("svg")
        |    .attr("class", "factorgraph")
        |    .attr("width", FG$fgid.width)
        |    .attr("height", FG$fgid.height)
        |    .style("overflow", "visible");
        |
        |
        |    FG$fgid.link = FG$fgid.svg.selectAll(".link")
        |    FG$fgid.node = FG$fgid.svg.selectAll(".fgshape")
        |    FG$fgid.label = FG$fgid.svg.selectAll(".label");
        |
        |    FG$fgid.force
        |    .nodes(FG$fgid.graph.nodes)
        |    .links(FG$fgid.graph.links)
        |    .start();
        |
        |    FG$fgid.link = FG$fgid.link.data(FG$fgid.graph.links)
        |	    .enter().append("line")
        |	    .attr("class", "link");
        |
        |	FG$fgid.node = FG$fgid.node.data(FG$fgid.graph.nodes)
        |	    .enter().append("path")
        |	    .attr("class", function(d) {
        |       return 'fgshape' +
        |         (d.type == 'factor' ? ' fgfactor' : ' fgnode') +
        |         (d.fixed ? ' fgfixed' : '');
        |     })
        |	    .attr("d", d3.svg.symbol()
        |	    	.type(function(d) { return d.type == 'factor' ? 'square' : 'circle' })
        |	    	.size(function(d) { return d.type == 'factor' ? 1000 : 2000 }))
        |	    .on("mouseover", function(d){
        |	    	if(d.hoverhtml != undefined) {
        |	    		FG$fgid.setTooltip(d.hoverhtml);
        |	    		FG$fgid.tooltip.transition()
        |	    			.duration(300)
        |	    			.style("opacity", .9);
        |	    		FG$fgid.tooltipNode = d;
        |	    		FG$fgid.moveTooltip();
        |	    	}
        |	    })
        |	    .on("mouseout", function(d){
        |			FG$fgid.tooltip.transition()
        |	                .duration(300)
        |	                .style("opacity", 0)
        |	    })
        |		.call(FG$fgid.drag);
        |
        |FG$fgid.label = FG$fgid.label.data(FG$fgid.graph.nodes)
        |	.enter().append("text")
        |	.attr("class", "label")
        |	.attr("dy", "5")
        |	.attr("text-anchor", "middle")
        |	.text(function(d) { return d.text == undefined ? "" : d.text })
        |	.call(FG$fgid.drag);
        |
        |FG$fgid.tooltipNode = null
        |FG$fgid.tooltip = null
        |
        |${
           /*if(!linear)*/ "while(FG" + fgid + ".force.alpha() > 0.05) {FG" + fgid + ".force.tick();}\n"
           //else "FG" + fgid + ".force.alpha(0);\n"
           }
        |
        |FG$fgid.tick = function() {
        |
        |    FG$fgid.node.each(function(d) {
        |     d.x = Math.max(0, Math.min($width, d.x));
        |     d.y = Math.max(0, Math.min($height, d.y));
        |    });
        |
        |	FG$fgid.link.attr("x1", function(d) { return d.source.x; })
        |		.attr("y1", function(d) { return d.source.y; })
        |		.attr("x2", function(d) { return d.target.x; })
        |		.attr("y2", function(d) { return d.target.y; });
        |
        |
        |	FG$fgid.node.attr("transform", function(d) {return "translate(" + d.x + "," + d.y + ")"});
        |	FG$fgid.label.attr("transform", function(d) {return "translate(" + d.x + "," + d.y + ")"});
        |	FG$fgid.moveTooltip();
        |
        |}
        |
        |FG$fgid.setTooltip = function(html) {
        |	if(FG$fgid.tooltip != null) {
        |		FG$fgid.tooltip.remove()
        |	}
        |	FG$fgid.tooltip = FG$fgid.svg.insert("foreignObject")
        |		.attr("class", "tooltip")
        |		.attr("width", "300")
        |		.attr("height", "100%")
        |		.style("opacity", 0)
        |		.html("<div class='tooltipinner'>" + html + "</div>")
        |}
        |
        |
        |FG$fgid.moveTooltip = function() {
        |	if(FG$fgid.tooltipNode != null) {
        |		FG$fgid.tooltip.attr("transform", "translate(" + (FG$fgid.tooltipNode.x-150) + "," + (FG$fgid.tooltipNode.y+15) + ")" );
        |	}
        |}
        |
        |FG$fgid.force.on("tick", FG$fgid.tick);
        |FG$fgid.tick();
        |}
        |
        |if(typeof d3 == "undefined") {
        |var head= document.getElementsByTagName('head')[0];
        |var script= document.createElement('script');
        |script.type= 'text/javascript';
        |script.src= 'http://d3js.org/d3.v3.min.js';
        |script.onload = onD3Loaded;
        |head.appendChild(script);
        |} else {
        |onD3Loaded();
        |}
        |</script>
        |</div>
      """.stripMargin

    RawHTML(code)
  }
}

/*
import java.awt._
import java.awt.image.BufferedImage
import java.io.{File, IOException}
import javax.imageio.ImageIO
import javax.swing._

import com.mxgraph.layout.mxFastOrganicLayout
import com.mxgraph.model.{mxGeometry, mxCell}
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.swing.util.mxMorphing
import com.mxgraph.view.mxGraph

/**
 * Created by Luke on 01/07/2014.
 */
class FactorGraphViewer {
  class MyMxGraph extends mxGraph {
    //can't move edges
    override def isCellSelectable(cell: AnyRef): Boolean = cell match {
      case c: mxCell => if (c.isEdge) false else super.isCellSelectable(c);
      case _ => super.isCellSelectable(cell);
    }
  }
  val f      = new JFrame
  val width  = 800
  val height = 600
  f.setSize(800, 600)
  f.setLocation(300, 200)
  val graph = new MyMxGraph
  graph.setAutoSizeCells(true)
  val graphComponent = new mxGraphComponent(graph)
  graphComponent.setConnectable(false)
  graphComponent.scrollToCenter(true)
  graph.setCellsResizable(false)
  f.getContentPane.add(BorderLayout.CENTER, graphComponent)
  f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  f.setVisible(true)
  val parent = graph.getDefaultParent

  def addFactor(text: String): AnyRef = {
    graph.updateCellSize(graph.insertVertex(parent, null, text, width / 4, height / 4, 0, 0))
  }

  def addGroupFactor(text: String): AnyRef = {
    graph.updateCellSize(graph.insertVertex(parent, null, text, width / 4, height / 4, 0, 0, "fillColor=green;fontColor=black"))
  }

  def addNode(text: String): AnyRef = {
    graph.insertVertex(parent, null, text, width / 4, height / 4, 40, 40, "shape=ellipse;fillColor=yellow")
  }

  def addEdge(v1: AnyRef, v2: AnyRef, text: String): AnyRef = {
    graph.insertEdge(parent, null, text, v1, v2, "fontColor=black")
  }

  def addEdge(v1: AnyRef, v2: AnyRef): AnyRef = {
    addEdge(v1, v2, "")
  }

  var nTextboxes = 0
  def addTextbox(text: String): AnyRef = {
    val t = graph.insertVertex(parent, null, text, width - 210, 10 + 50 * nTextboxes, 200, 50, "fontColor=black;fillColor=white;opacity=0")
    nTextboxes = nTextboxes + 1
    t
  }

  def render() = {
    for (i <- 0 until parent.asInstanceOf[mxCell].getChildCount) {
      val c = parent.asInstanceOf[mxCell].getChildAt(i)
      val x = width * i / parent.asInstanceOf[mxCell].getChildCount
      val g = c.getGeometry
      if (c.isVertex) c.setGeometry(new mxGeometry(x, g.getY, g.getWidth, g.getHeight))
    }
    val layout = new mxFastOrganicLayout(graph)
    layout.setForceConstant(100)
    layout.execute(graph.getDefaultParent)
    new mxMorphing(graphComponent).startAnimation()
  }

  def save(`type`: String, destination: String) {
    val content: Container = f.getContentPane
    val img: BufferedImage = new BufferedImage(content.getWidth, content.getHeight, BufferedImage.TYPE_INT_RGB)
    content.printAll(img.createGraphics)
    try {
      ImageIO.write(img, `type`, new File(destination))
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }
}
*/