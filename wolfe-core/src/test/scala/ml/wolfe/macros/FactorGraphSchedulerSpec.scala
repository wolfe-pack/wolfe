package ml.wolfe.macros

import ml.wolfe.{BeliefPropagation, FactorGraph, Wolfe}
import ml.wolfe.util.NLP
import Wolfe._
import NLP._
import OptimizedOperators._

import ml.wolfe.FactorGraph.{MPSchedulerImpl => SchedulerImpl, _}


/**
 * Created by rockt on 09/04/2014.
 */
class FactorGraphSchedulerSpec extends StructureIsomorphisms {
  def indexTuple(e:DirectedEdge) = e.direction match {
    case EdgeDirection.F2N => (e.f.index, e.n.index)
    case EdgeDirection.N2F => (e.n.index, e.f.index)
  }

  "A scheduler" should {
    "given a tree" should {
      val graph = new FactorGraph()
      (0 to 5).foreach(i => graph.addDiscreteNode(0)) //dummy nodes
      (0 to 104).foreach(i => graph.addFactor()) //dummy factors
      val edges = Array(
          100 -> 0, 100 -> 1,
          101 -> 2, 101 -> 3,
          102 -> 1, 102 -> 3, 102 -> 4,
          103 -> 4, 103 -> 5,
          104 -> 5
        )
      edges.foreach(edge => graph.addEdge(graph.getFactor(edge._1), graph.getNode(edge._2)))
      graph.build()
      val root = graph.nodes(2) //"randomly" pick a root node
      /*
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
      */
      val branch0 = Seq(0 -> 100, 100 -> 1, 1 -> 102)
      val branch1 = Seq(104 -> 5, 5 -> 103, 103 -> 4, 4 -> 102)
      val finalBranch = Seq(102 -> 3, 3 -> 101, 101 -> 2)

      "return the right edge ordering for a forward messaging pass" in {
        val predicted = SchedulerImpl.scheduleForward(root).map(indexTuple)
        val startLength = branch0.length + branch1.length
        //println(predicted.mkString("\n"))
        predicted take startLength intersect branch0 shouldEqual branch0
        predicted take startLength intersect branch1 shouldEqual branch1
        predicted drop startLength shouldEqual finalBranch
      }

      /*
      "return the right edge ordering for a backward messaging pass" in {
        val predicted = SchedulerImpl.schedule(root, Direction.Backward).map(e => (e.f.index, e.n.index))
        validBackwardPasses should contain(predicted)
      }

      "return the right edge ordering for a forward-backward messaging pass" in {
        val predicted = SchedulerImpl.schedule(root).map(e => (e.f.index, e.n.index))
        validForwardBackwardPasses should contain(predicted)
      }*/
    }

    "given a loopy graph" should {
      val graph = new FactorGraph()
      (0 to 2).foreach(i => graph.addDiscreteNode(0)) //dummy nodes
      (0 to 102).foreach(i => graph.addFactor()) //dummy factors
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
      val root = graph.nodes(1) //"randomly" pick a root node


      "return a tree-like ordering for a forward messaging pass" in {
        val predicted = SchedulerImpl.scheduleForward(root)

        val visitedNodes = collection.mutable.Set[Node]()
        val visitedFactors = collection.mutable.Set[Factor]()
        val head = predicted.head
        head.direction match {
          case EdgeDirection.F2N => visitedFactors += head.f
          case EdgeDirection.N2F => visitedNodes += head.n
        }

        for(e <- predicted) e.direction match {
          case EdgeDirection.F2N =>
            visitedFactors should contain (e.f)
            visitedNodes += e.n
          case EdgeDirection.N2F =>
            visitedNodes should contain (e.n)
            visitedFactors += e.f
        }

        (0 to 2).foreach(i => visitedNodes should contain(graph.nodes(i)))
        (100 to 102).foreach(i => visitedFactors should contain(graph.factors(i)))
      }

      /*
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
      }*/
    }

    "given a graph with disconnected components return a schedule on all components" in {
      val graph = new FactorGraph()
      (0 to 4).foreach(i => graph.addDiscreteNode(0)) //dummy nodes
      (0 to 101).foreach(i => graph.addFactor()) //dummy factors
      val edges = Array(
          100 -> 0,
          100 -> 1,
          101 -> 2,
          101 -> 3
        )
      edges.foreach(edge => graph.addEdge(graph.getFactor(edge._1), graph.getNode(edge._2)))
      graph.build()

      val predicted = SchedulerImpl.schedule(graph).map(indexTuple)
      predicted.length shouldEqual edges.length * 2
      predicted.containsSlice(Seq(0 -> 100, 100 -> 1)) || predicted.containsSlice(Seq(1 -> 100, 100 -> 0)) should be(true)
      predicted.containsSlice(Seq(2 -> 101, 101 -> 3)) || predicted.containsSlice(Seq(3 -> 101, 101 -> 2)) should be(true)
    }

    "given an inner node first return the forward passes and then the backward passes" in {
      val graph = new FactorGraph()
      (0 to 4).foreach(i => graph.addDiscreteNode(0)) //dummy nodes
      (0 to 101).foreach(i => graph.addFactor()) //dummy factors
      val edges = Array(
          100 -> 0,
          100 -> 1,
          101 -> 1,
          101 -> 2
        )
      edges.foreach(edge => graph.addEdge(graph.getFactor(edge._1), graph.getNode(edge._2)))
      graph.build()

      val root = graph.nodes(1)

      val branch0 = Seq(0 -> 100, 100 -> 1)
      val branch1 = Seq(2 -> 101, 101 -> 1)
      val back0 = branch0.reverse map (_.swap)
      val back1 = branch1.reverse map (_.swap)
      val startLength = branch0.length + branch1.length

      val predicted = SchedulerImpl.schedule(root).map(indexTuple)
      predicted take startLength intersect branch0 shouldEqual branch0
      predicted take startLength intersect branch1 shouldEqual branch1
      predicted drop startLength intersect back0 shouldEqual back0
      predicted drop startLength intersect back1 shouldEqual back1
    }

    "when turned off return edges in the order they were added" in {
      val graph = new FactorGraph()
      (0 to 4).foreach(i => graph.addDiscreteNode(0)) //dummy nodes
      (0 to 103).foreach(i => graph.addFactor()) //dummy factors
      val edges = Array(
          103 -> 1,
          100 -> 0,
          101 -> 1,
          101 -> 0,
          102 -> 1,
          102 -> 2
        )

      edges.foreach(edge => graph.addEdge(graph.getFactor(edge._1), graph.getNode(edge._2)))
      graph.build()

      val actual = Seq(103 -> 1, 100 -> 0, 0 -> 101, 101 -> 1, 2 -> 102, 102 -> 1)
      val predicted = SchedulerImpl.canonicalSchedule(graph).map(indexTuple)

      predicted shouldEqual actual
    }
  }
}
