package ml.wolfe.macros

import ml.wolfe.Wolfe
import Wolfe._


/**
 * @author Sebastian Riedel
 */
class ConditionerSpecs extends StructureIsomorphisms {

  "A conditioner" should {
    "condition an atomic sample space" in {
      val space = Seq(1, 2, 3, 4)
      val structure = Conditioner.conditioned(space)( (x: Int) => x == 1)
      structure mustBeIsomorphicTo (space filter (_ == 1))
    }
    "condition an atomic boolean sample space" in {
      val space = Seq(true, false)
      val structure = Conditioner.conditioned(space)( (x: Boolean) => x)
      structure mustBeIsomorphicTo (space filter (x => x))
    }
    "condition a complex sample space " in {
      implicit val persons = Seq("Sameer", "Vivek")
      case class Data(smokes: Pred[String])
      val space = Wolfe.all(Data)
      val structure = Conditioner.conditioned(space)((x: Data) => x.smokes("Sameer"))
      structure mustBeIsomorphicTo (space filter (_.smokes("Sameer")))
    }
    "condition a complex sample space with a conjunction" in {
      implicit val ints = Range(0, 4)
      val space = Wolfe.Pred[Int]
      val structure = Conditioner.conditioned(space)((x: Pred[Int]) => x(0) && x(2) && x(3))
      structure mustBeIsomorphicTo (space filter (x => x(0) && x(2) && x(3)))
    }
//    "condition using masks" in {
//      case class Data(x: Boolean, y: Boolean, z: Boolean)
//      val space = Wolfe.all(Data)
//      def observed(d: Data) = d.copy(y = mask[Boolean])
//      val observation = Data(true, true, false)
//      val conditioned = space filter (observed(_) == observed(observation))
//      val structure = Conditioner.conditioned(space)(observed(_) == observed(observation))
//      println(conditioned)
//    }

  }

}
