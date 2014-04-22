package ml.wolfe.macros

import ml.wolfe.{FactorGraph, Wolfe}
import ml.wolfe.util.NLP
import Wolfe._
import NLP._
import OptimizedOperators._
import ml.wolfe.FactorGraph.SchedulerImpl
import ml.wolfe.FactorGraph.SchedulerImpl.Direction


/**
 * Created by rockt on 09/04/2014.
 */
class FactorGraphSchedulerSpec extends StructureIsomorphisms {
  "A scheduler" should {
    "given a tree" should {
      val graph = new FactorGraph()
      (0 to 5).foreach(i => graph.addNode(0)) //dummy nodes
      (0 to 104).foreach(i => graph.addTableFactor(Array(), Array(), Array())) //dummy factors
      val edges = Array(
        100 -> 0, 100 -> 1,
        101 -> 2, 101 -> 3,
        102 -> 1, 102 -> 3, 102 -> 4,
        103 -> 4, 103 -> 5,
        104 -> 5
      )
      edges.foreach(edge => graph.addEdge(graph.getFactor(edge._1), graph.getNode(edge._2)))
      graph.build()
      val root = graph.edges(2) //"randomly" pick an edge that defines the root node

      val validForwardPasses = Set(
        Seq(104 -> 5, 100 -> 1, 103 -> 4, 102 -> 3),
        Seq(104 -> 5, 103 -> 4, 100 -> 1, 102 -> 3),
        Seq(100 -> 1, 104 -> 5, 103 -> 4, 102 -> 3)
      )

      val validBackwardPasses = Set(
        Seq(101 -> 3, 102 -> 1, 100 -> 0, 102 -> 4, 103 -> 5),
        Seq(101 -> 3, 102 -> 1, 102 -> 4, 100 -> 0, 103 -> 5),
        Seq(101 -> 3, 102 -> 1, 102 -> 4, 103 -> 5, 100 -> 0)
      )

      val validForwardBackwardPasses = for {
        forward <- validForwardPasses
        backward <- validBackwardPasses
      } yield forward ++ Seq((root.f.index, root.n.index)) ++ backward

      "return the right edge ordering for a forward messaging pass" in {
        val predicted = SchedulerImpl.schedule(root, Direction.Forward).map(e => (e.f.index, e.n.index))
        validForwardPasses should contain(predicted)
      }

      "return the right edge ordering for a backward messaging pass" in {
        val predicted = SchedulerImpl.schedule(root, Direction.Backward).map(e => (e.f.index, e.n.index))
        validBackwardPasses should contain(predicted)
      }

      "return the right edge ordering for a forward-backward messaging pass" in {
        val predicted = SchedulerImpl.schedule(root).map(e => (e.f.index, e.n.index))
        validForwardBackwardPasses should contain(predicted)
      }
    }

    "given a loopy graph" should {
      val graph = new FactorGraph()
      (0 to 2).foreach(i => graph.addNode(0)) //dummy nodes
      (0 to 102).foreach(i => graph.addTableFactor(Array(), Array(), Array())) //dummy factors
      val edges = Array(
          100 -> 0,
          100 -> 1,
          101 -> 1,
          101 -> 2,
          102 -> 2,
          102 -> 0
        )
      edges.foreach(edge => graph.addEdge(graph.getFactor(edge._1), graph.getNode(edge._2)))
      graph.build()
      val root = graph.edges(2) //"randomly" pick an edge that defines the root node

      "return only one loop for a forward messaging pass" in {
        val actual = Seq((101,1), (100,0), (102,2))
        val predicted = SchedulerImpl.schedule(root, Direction.Forward).map(e => (e.f.index, e.n.index))
        predicted should be(actual)
      }

      "return only one loop for a backward messaging pass" in {
        val actual = Seq((101,2), (102,0), (100,1))
        val predicted = SchedulerImpl.schedule(root, Direction.Backward).map(e => (e.f.index, e.n.index))
        predicted should be(actual)
      }


      "return a tree-like ordering for a forward/backward messaging pass" in {
        val actual = Set(
          Seq((100,1), (101,2), (102,0), (100,0), (102,2), (101,1)) //FIXME: list other possible cases
        )
        val predicted = SchedulerImpl.schedule(root).map(e => (e.f.index, e.n.index))
        actual should contain(predicted)
      }
    }

    "should given a graph with disconnected components return a schedule on all components" in {
      val graph = new FactorGraph()
      (0 to 4).foreach(i => graph.addNode(0)) //dummy nodes
      (0 to 101).foreach(i => graph.addTableFactor(Array(), Array(), Array())) //dummy factors
      val edges = Array(
          100 -> 0,
          100 -> 1,
          101 -> 2,
          101 -> 3
        )
      edges.foreach(edge => graph.addEdge(graph.getFactor(edge._1), graph.getNode(edge._2)))
      graph.build()

      val actual = Set(
        Seq(100 -> 0, 100 -> 1, 101 -> 2, 101 -> 3)
      )
      val predicted = SchedulerImpl.schedule(graph).map(e => (e.f.index, e.n.index))
      actual should contain(predicted)
    }

    "should given an inner node first return the forward passes and then the backward passes" in {
      val graph = new FactorGraph()
      (0 to 4).foreach(i => graph.addNode(0)) //dummy nodes
      (0 to 101).foreach(i => graph.addTableFactor(Array(), Array(), Array())) //dummy factors
      val edges = Array(
          100 -> 0,
          100 -> 1,
          101 -> 1,
          101 -> 2
        )
      edges.foreach(edge => graph.addEdge(graph.getFactor(edge._1), graph.getNode(edge._2)))
      graph.build()

      val root = graph.nodes(1)

      val actual = Set(
        Seq((101,1), (100,1), (100,0), (101,2))
      )
      val predicted = SchedulerImpl.schedule(root).map(e => (e.f.index, e.n.index))

      actual should contain(predicted)
    }
  }
}
