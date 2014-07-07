package ml.wolfe

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

