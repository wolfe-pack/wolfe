package ml.wolfe.macros

import ml.wolfe.Wolfe
import Wolfe._


/**
 * @author Sebastian Riedel
 */
class ConditionerSpecs extends StructureIsomorphisms {

  "A conditioner" should {
    "condition an atomic sample space using equality " in {
      def space = Seq(1, 2, 3, 4)
      val structure = Conditioner.conditioned(space)(x => x == 1)
      structure mustBeIsomorphicTo (space filter (_ == 1))
    }
    "condition an atomic boolean sample space using the boolean value itself" in {
      def space = Seq(true, false)
      val structure = Conditioner.conditioned(space)(x => x)
      structure mustBeIsomorphicTo (space filter (x => x))
    }
    "condition an case class sample space using equality " in {
      case class Data(x:Int)
      def space = Wolfe.all(Data)(0 until 4)
      val structure = Conditioner.conditioned(space)(x => x.x == 1)
      structure mustBeIsomorphicTo (space filter (_.x == 1))
    }
    "support import statements" in {
      case class Data(x:Boolean,y:Boolean)
      def space = Wolfe.all(Data)
      def predicate(i:Data) = {
        import i._
        x
      }
      val structure = Conditioner.conditioned(space)(predicate(_))
      structure mustBeIsomorphicTo (space filter (predicate(_)))
    }
    "condition a complex sample space " in {
      implicit val persons = Seq("Sameer", "Vivek")
      case class Data(smokes: Pred[String])
      def space = Wolfe.all(Data)
      val structure = Conditioner.conditioned(space)(x => x.smokes("Sameer"))
      structure mustBeIsomorphicTo (space filter (_.smokes("Sameer")))
    }
    "condition a complex sample space with a conjunction" in {
      implicit val ints = Range(0, 4)
      def space = Wolfe.Pred[Int]
      val structure = Conditioner.conditioned(space)(x => x(0) && x(2) && x(3))
      structure mustBeIsomorphicTo (space filter (x => x(0) && x(2) && x(3)))
    }
    "condition by specifying hidden variables in flat case class" in {
      case class Data(x: Boolean, y: Boolean, z: Boolean)
      def space = Wolfe.all(Data)
      def observed(d: Data) = d.copy(y = hidden)
      val instance = Data(true, true, false)
      val expected = space filter (observed(_) == observed(instance))
      val actual = Conditioner.conditioned(space)(observed(_) == observed(instance))
      actual mustBeIsomorphicTo expected
    }

    "condition by specifying hidden variables in nested case class" in {
      case class Nested(a: Boolean, b: Boolean)
      case class Data(x: Nested, y: Boolean, z: Boolean)
      implicit def nested = Wolfe.all(Nested)
      def space = Wolfe.all(Data)
      val instance = Data(Nested(false, false), true, false)
      def observed(d: Data) = d.copy(x = d.x.copy(a = hidden))
      val expected = space filter (observed(_) == observed(instance))
      val actual = Conditioner.conditioned(space)(observed(_) == observed(instance))
      actual mustBeIsomorphicTo expected
    }
    "condition by specifying hidden variables in case classes nested in a sequence " in {
      case class Element(a: Boolean, b: Boolean)
      def elements = Wolfe.all(Element)
      def space = seqs(elements, 4)
      val instance = Seq.fill(3)(Element(true, false))
      def observed(s: Seq[Element]) = s.map(_.copy(b = hidden))
      val expected = space filter (observed(_) == observed(instance))
      val actual = Conditioner.conditioned(space)(observed(_) == observed(instance))
      actual mustBeIsomorphicTo expected
    }
    "condition by specifying hidden variables in a sequence nested in a case class " in {
      case class Element(a: Boolean, b: Boolean)
      case class Container(elements: Seq[Element])
      def elements = Wolfe.all(Element)
      def space = Wolfe.all(Container)(seqs(elements, 4))
      val instance = Container(Seq.fill(3)(Element(true, false)))
      def observed(c: Container) = c.copy(elements = c.elements.map(_.copy(b = hidden)))
      val expected = space filter (observed(_) == observed(instance))
      val actual = Conditioner.conditioned(space)(observed(_) == observed(instance))
      actual mustBeIsomorphicTo expected
    }

    "condition by specifying hidden variables in flat case class using a library function" in {
      import Library._
      case class Data(x: Boolean, y: Boolean, z: Boolean)
      def space = Wolfe.all(Data)
      def observed(d: Data) = d.copy(y = hidden)
      val instance = Data(true, true, false)
      val expected = space filter evidence(observed)(instance)
      val actual = Conditioner.conditioned(space)(evidence(observed)(instance))
      actual mustBeIsomorphicTo expected
    }

    "turn infinite search spaces into finite structures when conditioning " in {
      case class Data(label:String)
      def space = infty[Data]
      def condition(d:Data) = d == Data("test")
      val actual = Conditioner.conditioned(space)(condition)
      actual mustBeIsomorphicTo Seq(Data("test"))
    }

  }

}
