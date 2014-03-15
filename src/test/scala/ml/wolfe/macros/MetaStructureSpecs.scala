package ml.wolfe.macros

import ml.wolfe.{Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class MetaStructureSpecs extends WolfeSpec {

  implicit class StructureTest[T](structure:Structure[T]) {
    def mustBeIsomorphicTo(iterable:Iterable[T]) {
      val expectedSize = iterable.size
      val expectedSet = iterable.toSet
      var count = 0
      structure.resetSetting()
      while (structure.hasNextSetting) {
        structure.nextSetting()
        val value = structure.value()
        expectedSet should contain (value)
        count += 1
      }
      count should be (expectedSize)
    }
  }
  implicit class StructureProjectionTest[T1,T2](pair:(Structure[T1],Structure[T1] => T2)) {
    def mustBeIsomorphicTo(that:(Iterable[T1],T1=>T2)) = {
      val iterable = that._1
      val structure = pair._1
      val expectedSize = iterable.size
      val expectedSet = iterable.toSet
      var count = 0
      structure.resetSetting()
      while (structure.hasNextSetting) {
        structure.nextSetting()
        val value = structure.value()
        val expectedProjection = that._2(value)
        val actualProjection = pair._2(structure)
        expectedSet should contain (value)
        actualProjection should be (expectedProjection)
        count += 1
      }
      count should be (expectedSize)
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
      import Wolfe._
      case class Data(x:Boolean,y:Boolean)
      val space = Wolfe.all(Data)
      val structure = MetaStructure.createStructure(space)
      structure mustBeIsomorphicTo space
      structure.nodes().size should be (2)
    }

//    "generate a structure for all case class objects within a cartesian product of arguments blah " in {
//      import Wolfe._
//      case class Data(x:Boolean,y:Boolean)
//      val space = Wolfe.all(Data)
//      val (structure,projection) = MetaStructure.createStructureAndProjection[Data,Boolean](space, d => d.x)
//      (structure,projection) mustBeIsomorphicTo (space,_.x)
//
//
////      structure mustBeIsomorphicTo space
////      structure.nodes().size should be (2)
//    }

  }

}
