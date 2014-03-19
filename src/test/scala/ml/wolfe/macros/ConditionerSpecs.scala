package ml.wolfe.macros

import ml.wolfe.Wolfe

/**
 * @author Sebastian Riedel
 */
class ConditionerSpecs extends StructureIsomorphisms {

  "A conditioner" should {
    "condition an atomic sample space" in {
      val space = Seq(1,2,3,4)
      val structure = Conditioner.conditioned(space, (x:Int) => x == 1)
      structure mustBeIsomorphicTo (space filter (_ == 1))
    }
    "condition an atomic boolean sample space" in {
      val space = Seq(true,false)
      val structure = Conditioner.conditioned(space, (x:Boolean) => x)
      structure mustBeIsomorphicTo (space filter (x => x))
    }
    "condition a complex sample space " in {
      import Wolfe._
      implicit val persons = Seq("Sameer","Vivek")
      case class Data(smokes:Pred[String])
      val space = Wolfe.all(Data)
      val structure = Conditioner.conditioned(space, (x:Data) => x.smokes("Sameer"))
      structure mustBeIsomorphicTo (space filter (_.smokes("Sameer")))
    }


  }

}
