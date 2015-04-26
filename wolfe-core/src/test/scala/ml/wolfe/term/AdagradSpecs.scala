package ml.wolfe.term

import ml.wolfe._
import Argmaxer._
import ml.wolfe.util.Math._

/**
 * @author riedel
 */
class AdagradSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  implicit val params = AdaGradParameters(100, 1)

  "Adagrad" should {
    "optimize a quadratic objective" in {
      val x = Doubles.Var
      def obj(x: Doubles.Term) = x * 4.0 - x * x
      val result = argmax(Doubles) { x => obj(x)} by adaGrad
      result.eval() should be(2.0 +- eps)
    }
    "optimize a multivariate quadratic objective" in {
      val X = Vectors(2)
      def obj(x: X.Term) = (x dot vector(4.0, 4.0)) - (x dot x)
      val result = argmax(X) { x => obj(x) } by adaGrad
      result.eval() should equal(vector(2.0, 2.0))
    }
    "optimize a sampled quadratic objective" ignore {
      import TermImplicits.Seqs
      implicit val rand = ml.wolfe.util.Math.random

      @domain case class Theta(params: IndexedSeq[Vect])
      implicit val Thetas = Theta.Values(Seqs(Vectors(1), 3))

      @domain case class User(items: IndexedSeq[Int])
      implicit val Items = Ints(0 until 4)
      implicit val Users = User.Values(Seqs(Items, 0, 2))
      val users = Seq(User(IndexedSeq()), User(IndexedSeq(1, 2))).toConst

      def loss(t: Thetas.Term): DoubleTerm = {
        val user = mem(users.sampleShuffled)
        sum(user.items) { ix => { val x = t.params(ix); (x(0) * 4.0) - (x dot x) }}
        //sum(user.items) { ix => { val x = t.params(ix); (x(0) * 4.0) - (x(0) * x(0)) }} //fixme: why does this give a different result?
      }

      val init = Settings(Thetas.createRandomSetting(random.nextGaussian()))
      val thetaStar = (argmax(Thetas) { t => loss(t) } by adaGrad(AdaGradParameters(1000, 0.1, initParams = init))).eval()
      thetaStar.params(1)(0) should be(2.0 +- eps)
      thetaStar.params(2)(0) should be(2.0 +- eps)
    }
  }


}
