package ml.wolfe.macros

import ml.wolfe.{Wolfe, WolfeSpec}
import scala.collection.mutable
import Wolfe._


/**
 * @author Sebastian Riedel
 */
class MetaStructureSpecs extends WolfeSpec {

  implicit class StructureTest[T](structure:Structure[T]) {
    def mustBeIsomorphicTo(expected:Iterable[T]) {
      val actual = new mutable.ArrayBuffer[T]()
      structure.resetSetting()
      while (structure.hasNextSetting) {
        structure.nextSetting()
        val value = structure.value()
        actual += value
      }
      actual.size should be (expected.size)
      actual.toSet should be (expected.toSet)

      for (t <- expected) {
        structure.observe(t)
        structure.resetSetting()
        structure.nextSetting()
        structure.value() should be (t)
      }

    }
  }
  implicit class StructureProjectionTest[T1,T2](pair:(Structure[T1],Structure[T1] => T2)) {
    def mustBeIsomorphicTo(that:(Iterable[T1],T1=>T2)) = {
      val expected = that._1
      val structure = pair._1
      val actual = new mutable.ArrayBuffer[T1]()
      structure.resetSetting()
      while (structure.hasNextSetting) {
        structure.nextSetting()
        val value = structure.value()
        val expectedProjection = that._2(value)
        val actualProjection = pair._2(structure)
        actualProjection should be (expectedProjection)
        actual += value
      }
      actual.size should be (expected.size)
      actual.toSet should be (expected.toSet)

      for (t <- expected) {
        structure.observe(t)
        structure.resetSetting()
        structure.nextSetting()
        structure.value() should be (t)
      }

    }
  }


  "A meta structure" should {
    "generate a structure for an atomic sequence of values" in {
      val space = Seq(false,true)
      val structure = MetaStructure.createStructure(space)
      structure mustBeIsomorphicTo space
      structure.nodes().size should be (1)
    }
    "generate a matching projection for an atomic sequence" in {
      val space = Seq(false,true)
      val (structure,projection) = MetaStructure.createStructureAndProjection[Boolean,Boolean](space, x => x)
      (structure,projection) mustBeIsomorphicTo (space,x => x)
    }
    "generate a structure for all case class objects within a cartesian product of arguments" in {
      case class Data(x:Boolean,y:Boolean)
      val space = Wolfe.all(Data)
      val structure = MetaStructure.createStructure(space)
      structure mustBeIsomorphicTo space
      structure.nodes().size should be (2)
    }

    "generate isomorphic structure and projection for case class spaces" in {
      case class Data(a1:Boolean,a2:Boolean)
      val space = Wolfe.all(Data)
      val (structure,projection) = MetaStructure.createStructureAndProjection[Data,Boolean](space, d => d.a1)
      (structure,projection) mustBeIsomorphicTo (space,_.a1)
    }

    "generate isomorphic structure and projection for predicate spaces" in {
      implicit val ints = Range(0,2)
      val space = Wolfe.Pred[(Int,Int)]
      val (structure,projection) = MetaStructure.createStructureAndProjection[Pred[(Int,Int)],Boolean](space, d => d(1,1))
      (structure,projection) mustBeIsomorphicTo (space,d => d(1,1))

    }

  }

}
