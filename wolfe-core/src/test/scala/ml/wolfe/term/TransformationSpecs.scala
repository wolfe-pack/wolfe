package ml.wolfe.term

import ml.wolfe.{Language, Tensor}

/**
 * @author riedel
 */
class TransformationSpecs extends WolfeSpec {

  import Language._

  "Shattering" should {
    "decompose structured variables into constructed terms of atomic access paths to the structured variable" in {
      @termdef case class Test(a: Double, b: Double)
      val x = Var[Test]
      val t = x.a
      val domains = Typer.domains(x in Test.Dom(Doubles, Doubles))(t).get
      val s = Transformation.shatter(t, domains)
      val expected = Test.Term(VarAccess(x, List(GetElement(x,0), x)), VarAccess(x, List(GetElement(x,1), x))).a

      s should beStringEqual (expected)
    }


  }
}
