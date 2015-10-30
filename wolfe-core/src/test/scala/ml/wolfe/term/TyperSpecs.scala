package ml.wolfe.term

import ml.wolfe.{Tensor, Language}
import org.scalautils.Good

/**
 * @author riedel
 */
class TyperSpecs extends WolfeSpec {

  import Language._

  "A typer" should {
    "infer Matrix * vector multiplication dimensions" in {
      val W = Var[Tensor]
      val x = Var[Tensor]
      val term = W * x
      val domains = Typer.domains(Domains(W in TensorDom(List(3,4)), x in TensorDom(List(4))))(term)
      domains.get(term) should be (TensorDom(List(3)))
    }
    "infer Matrix * Matrix multiplication dimensions" in {
      val W = Var[Tensor]
      val V = Var[Tensor]
      val term = W * V
      val domains = Typer.domains(Domains(W in TensorDom(List(3,4)), V in TensorDom(List(4,5))))(term)
      domains.get(term) should be (TensorDom(List(3,5)))
    }

    "infer domains for domain preserving operators" in {
      val W = Var[Tensor]
      val term = sigmoid(W)
      Typer.domains(W in TensorDom(List(3,4)))(term).get(term) should be (TensorDom(List(3,4)))
    }

    "infer a product domain from a case class value" in {
      @termdef case class Test(a:Double, b:Double)

      val value = Test(1.0, 2.0)
      val ProductDom(doms,construct) = Typer.deriveDomainFromValue(value)

      doms should be (List(Doubles,Doubles))
      construct(Seq(1.0,2.0)) should be (Test(1.0,2.0))

    }


  }
}
