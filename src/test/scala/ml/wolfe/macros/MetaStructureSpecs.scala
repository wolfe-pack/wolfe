package ml.wolfe.macros

import ml.wolfe.{Wolfe, WolfeSpec}
import scala.collection.mutable
import Wolfe._


/**
 * @author Sebastian Riedel
 */
class MetaStructureSpecs extends StructureIsomorphisms{

  "A meta structure" should {
    "generate a structure for an atomic sequence of values" in {
      val space = Seq(false,true)
      val structure = MetaStructure.structure(space)
      structure mustBeIsomorphicTo space
      structure.nodes().size should be (1)
    }
    "generate a matching projection for an atomic sequence" in {
      val space = Seq(false,true)
      val (structure,projection) = MetaStructure.projection[Boolean,Boolean](space, x => x)
      (structure,projection) mustBeIsomorphicTo (space,x => x)
    }
    "generate a structure for all case class objects within a cartesian product of arguments " in {
      case class Data(x:Boolean,y:Boolean)
      val space = Wolfe.all(Data)
      val structure = MetaStructure.structure(space)
      structure mustBeIsomorphicTo space
      structure.nodes().size should be (2)
    }

    "generate isomorphic structure and projection for case class spaces" in {
      case class Data(a1:Boolean,a2:Boolean)
      val space = Wolfe.all(Data)
      val (structure,projection) = MetaStructure.projection[Data,Boolean](space, d => d.a1)
      (structure,projection) mustBeIsomorphicTo (space,_.a1)
    }

    "generate isomorphic structure and projection for predicate spaces with one argument" in {
      implicit val ints = Range(0,2)
      val space = Wolfe.Pred[Int]
      val (structure,projection) = MetaStructure.projection[Pred[Int],Boolean](space, d => d(1))
      (structure,projection) mustBeIsomorphicTo (space,d => d(1))
      structure.nodes().size should be (2)
    }

    "generate isomorphic structure and projection for predicate spaces with more than 1 argument" in {
      implicit val ints = Range(0,2)
      val space = Wolfe.Pred[(Int,Int)]
      val (structure,projection) = MetaStructure.projection[Pred[(Int,Int)],Boolean](space, d => d(1,1))
      (structure,projection) mustBeIsomorphicTo (space,d => d(1,1))
      structure.nodes().size should be (4)
    }

  }

}

