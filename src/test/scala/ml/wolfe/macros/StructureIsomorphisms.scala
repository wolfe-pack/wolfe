package ml.wolfe.macros

import ml.wolfe.WolfeSpec
import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
trait StructureIsomorphisms extends WolfeSpec {

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

  implicit class StructuredFactorTest[T](factor:StructuredFactor[T]) {
    def mustBeIsomorphicTo(potential:T => Double) = {
      val structure = factor.structure
      structure.resetSetting()
      while (structure.hasNextSetting) {
        structure.nextSetting()
        val value = structure.value()
        val expected = potential(value)
        val actual = factor.potential()
        actual should be (expected)
      }
    }
  }

}