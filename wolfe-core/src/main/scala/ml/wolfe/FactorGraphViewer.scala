package ml.wolfe

import javax.imageio.ImageIO
import javax.swing._
import java.awt._
import java.awt.image.BufferedImage
import java.io.File
import java.io.IOException
import com.mxgraph.layout._
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.swing.util.mxMorphing
import com.mxgraph.view.mxGraph

/**
 * Created by Luke on 01/07/2014.
 */
class FactorGraphViewer {
  val f = new JFrame
  f.setSize(800, 600)
  f.setLocation(300, 200)
  val graph = new mxGraph
  val graphComponent = new mxGraphComponent(graph)
  graphComponent.setConnectable(false)
  graph.setCellsResizable(false)
  f.getContentPane.add(BorderLayout.CENTER, graphComponent)
  f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  f.setVisible(true)
  val parent = graph.getDefaultParent

  def addFactor(text: String): AnyRef = {
    graph.insertVertex(parent, null, text, 10, 10, 80, 80)
  }

  def addGroupFactor(text: String): AnyRef = {
    graph.insertVertex(parent, null, text, 10, 10, 100, 100, "fillColor=green;fontColor=black")
  }

  def addNode(text: String): AnyRef = {
    graph.insertVertex(parent, null, text, 10, 10, 50, 50, "shape=ellipse;fillColor=yellow")
  }

  def addEdge(v1: AnyRef, v2: AnyRef, text: String): AnyRef = {
    graph.insertEdge(parent, null, text, v1, v2)
  }

  def addEdge(v1: AnyRef, v2: AnyRef): AnyRef = {
    addEdge(v1, v2, "")
  }

  def render () = {
    new mxFastOrganicLayout(graph).execute(graph.getDefaultParent)
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

