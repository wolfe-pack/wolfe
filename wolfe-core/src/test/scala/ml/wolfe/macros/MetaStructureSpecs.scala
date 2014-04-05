package ml.wolfe.macros

import ml.wolfe.{Wolfe, WolfeSpec}
import scala.collection.mutable
import Wolfe._


/**
 * @author Sebastian Riedel
 */
class MetaStructureSpecs extends StructureIsomorphisms {

  "A meta structure" should {
    "generate a isomorphic structure and projection for an atomic sequence of values" in {
      def space = Seq(false, true)
      val (structure, projection) = MetaStructure.projection[Boolean, Boolean](space, x => x)
      (structure, projection) mustBeIsomorphicTo(space, x => x)
      structure.nodes().size should be(1)
    }

    "generate a structure for a double space that will be observed" in {
      val structure = MetaStructure.structure(doubles)
      structure.observe(1.0)
      structure.value() should be(1.0)
    }

    "generate a structure for a string space that will be observed" in {
      val structure = MetaStructure.structure(strings)
      structure.observe("Test")
      structure.value() should be("Test")
    }

    "generate a structure for all case class objects within a cartesian product of arguments" in {
      case class Data(x: Boolean, y: Boolean)
      def space = Wolfe.all(Data)
      val structure = MetaStructure.structure(space)
      structure mustBeIsomorphicTo space
      structure.nodes().size should be(2)
    }

    "generate a structure for all case class objects with a cartesian product builder" in {
      case class Data(x: Boolean, y: Boolean, z:Boolean)
      def space = Wolfe.all(Data)(bools x bools x bools)
      val structure = MetaStructure.structure(space)
      structure mustBeIsomorphicTo space
      structure.nodes().size should be(3)
    }


    "generate isomorphic structure and projection for case class spaces" in {
      case class Data(a1: Boolean, a2: Boolean)
      def space = Wolfe.all(Data)
      val (structure, projection) = MetaStructure.projection[Data, Boolean](space, d => d.a1)
      (structure, projection) mustBeIsomorphicTo(space, _.a1)
    }

    "generate isomorphic structure and projection for case class spaces in different files " in {
      def space = Wolfe.all(DifferentCompilationUnit.Data)
      val (structure, projection) = MetaStructure.projection[DifferentCompilationUnit.Data, Boolean](space, d => d.x)
      (structure, projection) mustBeIsomorphicTo(space, _.x)
    }


    "generate isomorphic structure and projection for predicate spaces with one argument" in {
      implicit val ints = Range(0, 2)
      def space = Wolfe.Pred[Int]
      val (structure, projection) = MetaStructure.projection[Pred[Int], Boolean](space, d => d(1))
      (structure, projection) mustBeIsomorphicTo(space, d => d(1))
      structure.nodes().size should be(2)
    }

    "generate isomorphic structure and projection for predicate spaces with more than 1 argument" in {
      implicit val ints = Range(0, 2)
      def space = Wolfe.Pred[(Int, Int)]
      val (structure, projection) = MetaStructure.projection[Pred[(Int, Int)], Boolean](space, d => d(1, 1))
      (structure, projection) mustBeIsomorphicTo(space, d => d(1, 1))
      structure.nodes().size should be(4)
    }

    "generate isomorphic structure and projection for fixed length sequence spaces" in {
      implicit val ints = Range(0, 2)
      def space = Wolfe.seqs(4, ints)
      val (structure, projection) = MetaStructure.projection[Seq[Int], Int](space, s => s(1))
      (structure, projection) mustBeIsomorphicTo(space, s => s(1))
      structure.nodes().size should be(4)
      structure.isInstanceOf[SeqStructure[Seq[Int]]] should be(true)
    }

    "generate structure and projection for max length length sequence spaces" in {
      implicit val ints = Range(0, 2)
      def space = Wolfe.seqs(ints, 4)
      val (structure, projection) = MetaStructure.projectionNoSetup[Seq[Int], Int](space, s => s(1))
      structure.isInstanceOf[SeqStructure[Seq[Int]]] should be(true)
      structure.asInstanceOf[SeqStructure[Seq[Int]]].setLength(3)
      structure.graph.setupNodes()
      (structure, projection) mustBeIsomorphicTo(space filter (_.size == 3), s => s(1))
      structure.nodes().size should be(3)
    }

  }

}

