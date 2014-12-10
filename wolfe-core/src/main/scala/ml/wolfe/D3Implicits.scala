package ml.wolfe

import java.io.{File, PrintWriter}
import java.util.UUID

import ml.wolfe.FactorGraph.{DirectedEdge, EdgeDirection, Factor, Node}
import ml.wolfe.Wolfe.FactorGraphBuffer
import ml.wolfe.fg.{TupleMsgs, DiscreteMsgs, TupleVar, DiscreteVar}
import org.sameersingh.htmlgen.Custom.Matrix
import org.sameersingh.htmlgen.{DivConverter, RawHTML, HTML}
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
        |	<div id="$id" style="display:inline-block;">
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
    val nonZero = v.filter(_._2 != 0).sortBy(_.toString())

      val width = 500

      val barHeight = 30
      val barSpace = 10

      val offset = 100

      val data = s"var data = { ${nonZero.map(x => "\"" + escape(x._1.toString) + "\": " + BigDecimal(x._2).setScale(3, BigDecimal.RoundingMode.HALF_UP)).mkString(", ")} };"
      val run =
        s"""
|				var barHeight = $barHeight;
|				var barSpace = $barSpace;
|
|				var svg = d3.select("#$id").append("svg")
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
    val charge = -200
    val linkDistance = 50
    val linkStrength = 0.5

    val strokeWidth = 2

    def nodeX(n:Node) = width * (
      if(nodeToNumber(n) == -1) Math.random()
      else ((nodeToNumber(n)-minNodeNumber).toDouble+0.5)/(maxNodeNumber - minNodeNumber +1))
    def nodeY(n:Node) = height * (
      if(nodeToNumber(n) == -1) Math.random()
      else (nodeTypes.indexOf(nodeToType(n))+1).toDouble / (nodeTypes.length+1))
    def factorX(f:Factor) = f.edges.map(e => nodeX(e.n)).sum / f.edges.length
    def factorY(f:Factor) = f.edges.map(e => nodeY(e.n)).sum / f.edges.length + 20
    def isFixed(n:Node) = linear && maxNodeNumber != -1 && (nodeToNumber(n) == minNodeNumber || nodeToNumber(n) == maxNodeNumber)

/*    def nodesMsgs = fg.visualizationMessages.map(
      set => set.map(
        e => e.direction match {
        case EdgeDirection.F2N if e.edge.msgs.isInstanceOf[DiscreteMsgs] => (e.edge.n, e.edge.msgs.asDiscrete.f2n)
        case EdgeDirection.N2F if e.edge.msgs.isInstanceOf[DiscreteMsgs] => (e.edge.n, e.edge.msgs.asDiscrete.n2f)
        case EdgeDirection.F2N if e.edge.msgs.isInstanceOf[TupleMsgs] => (e.edge.n, e.edge.msgs.asTuple.f2n.array)
        case EdgeDirection.N2F if e.edge.msgs.isInstanceOf[TupleMsgs] => (e.edge.n, e.edge.msgs.asTuple.n2f.array)
        case _ => null
    }))*/
    def msgHtml(e:DirectedEdge, m:Seq[Double]) = {
        val dom:Seq[String] = e.n.variable match {
          case t:DiscreteVar[_] => t.domain.map(_.toString)
          case t:TupleVar => t.domain
        }
        val headerRow = "<tr><td><i>" + e.n.variable.label + "</i></td><td></td></tr>"
        val tableRows = for((x, y) <- dom.zip(m)) yield {
          "<tr><td>" + x + "</td><td>" + "%.3f".format(y) + "</td></tr>"
        }
        "<table class='potentialtable'>" + headerRow + "\n" + tableRows.mkString("\n") + "</table>"
    }


    val style =
      s"""
        |.link {
        |	stroke: #000;
        |	stroke-width:${strokeWidth}px;
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
        |
        |marker{
        |	overflow:visible;
        |}
        |
        |
        |.factorgraph{
        | float:left;
        | clear:left;
        | }
        """.stripMargin

    val data =  s"""
        |var data = {graph:{
        |  "nodes": [${(
              nodes.map(n =>
                "{text:'" + escape(n.variable.label) + "'" +
                ", type:'" + (if(n.variable.isObserved) "observednode" else "node") + "'" +
                 ", hoverhtml:'<table style=\\'width:initial\\'><tr><td>Setting</td><td>Belief</td></tr>" + escape(
                  n.variable match {

                    case v:DiscreteVar[_] => (v.domain zip v.b).zipWithIndex.map {
                      case(db:(Any, Double),i:Int) =>
                        "<tr><td>" +
                        (if(v.fixedSetting && i == v.setting) s"<span style='font-weight:bold; color:#fc0'>${db._1}</span>" else db._1) +
                        s"</td><td>${db._2}</td></tr>"
                    }.mkString("\n")

                    case v:TupleVar => (v.domain zip v.b).zipWithIndex.map {
                      case(db:(Any, Double),i:Int) =>
                        "<tr><td>" +
                        db._1 +
                        s"</td><td>${db._2}</td></tr>"
                    }.mkString("\n")

                    case _ => ""
                  })
                + "</table>'" +
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

        |
        |    force
        |     .nodes(data.graph.nodes)
        |     .links(data.graph.links)
        |     .start();
        |
        |   var defs = svg.append("svg:defs")
        |
        |    var link = link.data(data.graph.links)
        |	    .enter().append("line")
        |	    .attr("class", "link")
        |
        |
        |    link.each( function(d, i) {
        |    defs.append("svg:marker")
        |    .attr("id", "markerArrow" + i)
        |    .attr("class", "markerArrow")
        |    .attr("markerWidth", "100")
        |    .attr("markerHeight", "100")
        |    .attr("refX", 0)
        |    .attr("orient", "auto")
        |    .append("svg:path")
        |     .attr("d", d3.svg.symbol()
        |       .type('triangle-up')
        |       .size(30))
        |     .attr("fill", "white");
        |   });
        |
        |	  var node = node.data(data.graph.nodes)
        |	    .enter().append("path")
        |	    .attr("class", function(d) {
        |       return 'fgshape ' +
        |         (d.type == 'factor' ? 'fgfactor' : d.type == 'observednode' ? 'fgnode_observed' : 'fgnode') +
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
        |		.attr("width", "350")
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
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |var samples = ${if (fg.visualizationSamples == null) "null" else
          fg.visualizationSamples.map{case(v,s,a) => "{node:" + v.node.index + ", sample:'" + escape(s.toString) +
            "', accepted:" + a + "}\n"}.mkString("[", ", ", "]")
          };
        |var toast = function(t, x, y) {
        | t.attr("opacity", 1)
        | .attr("x", x)
        | .attr("y", y)
        | .transition()
        | .duration(1000)
        | .ease("linear")
        | .attr("opacity", 0)
        | .attr("y", y-40)
        |}
        |var showSamples = function(i) {
        |  if(playing) {
        |   var n = data.graph.nodes[samples[i].node]
        |   var t = svg.append("text")
        |     .text(samples[i].sample)
        |     .style("font-weight", "bold")
        |     .attr("fill", samples[i].accepted  ? "#fc0" : "#f50")
        |     .attr("text-anchor", "middle")
        |   toast(t, n.x, n.y-30);
        |   setTimeout(function() {showSamples(i+1);}, 400)
        |  }
        |}
        |var schedule = ${if(fg.visualizationMessages == null) "null" else
            fg.visualizationMessages.map { _.map{
                case (e, m) =>
                  "{edge:" + fg.edges.indexOf(e.edge) + ", direction:'" + e.direction + "', msg:'" + escape(msgHtml(e,m)) + "'}"
              }.mkString("[", ", ", "]")
            }.mkString("[", ", ", "]")
          };
        |var transitionIndex = 0;
        |var playing = false;
        |var playTransition = function(){
        | if(transitionIndex >= schedule.length) {
        |   transitionIndex = 0;
        |   playing = false;
        |   playbtn.text("Play");
        |   //playTransition();
        | } else {
        |   var es = schedule[transitionIndex];
        |   link.attr("marker-end", "none");
        |   var finCount = 0;
        |   es.forEach(function(e) {
        |     var l = link[0][e.edge];
        |     var dx = l.getAttribute("x1") - l.getAttribute("x2");
        |     var dy = l.getAttribute("y1") - l.getAttribute("y2");
        |     var len = Math.sqrt(dx * dx + dy * dy) / ${strokeWidth}
        |
        |     d3.select(link[0][e.edge])
        |       .attr("marker-end", "url(#markerArrow" + e.edge + ")");
        |
        |     d3.select("#markerArrow" + e.edge)
        |         .attr("refX", (e.direction == 'N2F' ? len : 0))
        |         .select("path")
        |           .attr("transform", "rotate(" + (e.direction == 'N2F' ? 90 : -90) + ")");
        |
        |     d3.select("#markerArrow" + e.edge)
        |         .transition()
        |         .attr("refX", (e.direction == 'N2F' ? 0 : len))
        |         .duration(1000)
        |         .ease("linear");
        |
        |     d3.select(link[0][e.edge]).each(function(d) {
        |           var t = d3.select(this).transition().duration(1000).each("end", function() {
        |             d.msgVisited = !d.msgVisited;
        |             if(++finCount == es.length) {
        |               transitionIndex++;
        |               playTransition();
        |             }
        |           });
        |           t.style("stroke", d.msgVisited ? "black" : "white");
        |     }).on("mouseover", function(d) {
        |   		  setTooltip(e.msg);
        |   		  tooltip.transition()
        |   			  .duration(300)
        |   			  .style("opacity", .9);
        |   		  tooltipNode = d.source;
        |   		  moveTooltip();
        |     })
        |     .on("mouseout", function(d) {
        |	      tooltip.transition()
        |           .duration(300)
        |           .style("opacity", 0)
        |     });
        |   });

        |   }
        |
        |}
        |var playbtn = null;
        |if(schedule != null || samples != null) {
        | playbtn = d3.select("#$id")
        |   .insert("div", "svg")
        |   .attr("class", "playbtn")
        |   .text("Play")
        |   .on('click', function() {
        |      if(playing == false) {
        |        d3.select(this).text("Pause");
        |        playing = true;
        |        if(schedule != null) { playTransition(); } else { showSamples(0);}
        |      } else {
        |        playing = false;
        |        d3.select(this).text("Play");
        |        if(schedule != null) {
        |         svg.selectAll(".markerArrow").transition();
        |         link.transition();
        |        }
        |      }
        |   });
        |}
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

  def factorGraphURL(implicit fg:FactorGraphBuffer) = new HTML {
    val dir = new File("public/docs/tmp/")
    val tmp = new File(dir,"factorgraph_" + UUID.randomUUID().toString + ".html")
    dir.mkdir()
    tmp.deleteOnExit()
    saveD3Graph(fg.get(),tmp.getAbsolutePath)
    def source = s"""<a href="/assets/docs/tmp/${tmp.getName}">Fullscreen Factor Graph</a>"""
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
        "https://raw.githubusercontent.com/wolfe-pack/moro/master/public/stylesheets/wolfe.css" +
        //  "/home/luke/workspace/moro/public/stylesheets/wolfe.css" +
    "' />" +
    "</head>" +
    "<body>" + html.source + "</body>" +
    "</html>")

    writer.close()
  }

  // ------ Image Processing ------
  import java.awt.Image
  import java.awt.image.BufferedImage
  import java.net.URL
  import javax.imageio.ImageIO
  import javax.swing.ImageIcon
  import java.awt.FlowLayout
  import javax.swing.{JLabel, JFrame}

  def readImage(url: String): BufferedImage = {
    val img = ImageIO.read(new URL(url))
    img
  }

  def displayImage(img: BufferedImage): Unit = {
    val icon = new ImageIcon(img)
    val frame = new JFrame()
    frame.setLayout(new FlowLayout())
    frame.setSize(img.getWidth + 10, img.getHeight + 35)
    val lbl = new JLabel()
    lbl.setIcon(icon)
    frame.add(lbl)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }

  def resizeImage(img: BufferedImage, newW: Int) = {
    val ratio = img.getHeight.toDouble / img.getWidth.toDouble
    val tmp = img.getScaledInstance(newW, math.floor(ratio * 100).toInt, Image.SCALE_SMOOTH);
    val dimg = new BufferedImage(newW, math.floor(ratio * 100).toInt, BufferedImage.TYPE_INT_ARGB);

    val g2d = dimg.createGraphics()
    g2d.drawImage(tmp, 0, 0, null)
    g2d.dispose()

    dimg
  }

  def binarizeImage(img: BufferedImage): BufferedImage = {
    val gray = new BufferedImage(img.getWidth(), img.getHeight(),
      BufferedImage.TYPE_BYTE_BINARY)

    val g = gray.createGraphics()
    g.drawImage(img, 0, 0, null)
    gray
  }

  def saveImage(img: BufferedImage, path: String): Unit = {
    val outputfile = new File(path)
    ImageIO.write(img, "png", outputfile)
  }

  def imgToMatrix(img: BufferedImage): Matrix[Double] = {
    val pixels = (0 until img.getHeight).map(y => {
      (0 until img.getWidth).map(x => {
        val pixel = img.getRGB(x,y)
        // extract each color component
        val red   = (pixel >>> 16) & 0xFF
        val green = (pixel >>>  8) & 0xFF
        val blue  = (pixel >>>  0) & 0xFF

        // calc luminance in range 0.0 to 1.0; using SRGB luminance constants
        val luminance = (red * 0.2126f + green * 0.7152f + blue * 0.0722f) / 255.toDouble
        1.0-luminance
      })
    })
    Matrix(pixels)
  }

  def imageURLToMatrix(url: String, width: Int = 50) = imgToMatrix(resizeImage(readImage(url), width))

  case class Img(url: String)

  implicit def imageToHTML(img: Img): HTML = DivConverter.convert(imageURLToMatrix(img.url))
}
