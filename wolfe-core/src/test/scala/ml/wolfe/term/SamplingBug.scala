package ml.wolfe.term

import ml.wolfe._
import ml.wolfe.util.Math._

/**
 * @author rockt
 */
object SamplingBug extends App {
  import TermImplicits._
  @domain case class Theta(rs: IndexedSeq[FactorieVector], ps: IndexedSeq[FactorieVector])
  val theta = Theta.Dom(seqs(vectors(2), 3), seqs(vectors(2), 4))

  val pairIx: Map[(Any,Any), Int] = Map((0, 0) -> 0, (0, 1) -> 1)

  def dynPairIx(e1: Dynamic[Any], e2: Dynamic[Any]) = new Dynamic[Int] {
    def value(): Int = pairIx(e1.value() -> e2.value())
  }

  def stochasticPairIx(e1: Dynamic[Any], e2: Dynamic[Any]) = new Dynamic[Int] {
    def value(): Int = random.shuffle(pairIx.values).head
  }

  def dynPairGenerator[T](e1: Dynamic[Any], e2: Dynamic[Any]) = new DynamicGenerator[Int] {
    var _current = 0
    def generateNext() = {
      val v1 = e1.value()
      val v2 = e2.value()
      //do something stochastic in here
      _current = pairIx(v1 -> v2)
    }

    val value = new Dynamic[Int] {
      def value() = _current
    }
  }



  def loss(data: IndexedSeq[(Int, Int, Int)])(t: theta.Var) =
    stochasticTerm(stochastic(data)) { case unapply3(s, i, j) =>
      val r = t.rs(s)
      val p = t.ps(dynPairIx(i, j))
      val pSampled = t.ps(stochasticPairIx(i, j))
      (r dot p) + stochasticTerm(dynPairGenerator(i,j)) {k => r dot t.ps(k)}
//      (r dot p) + (r dot pSampled)
    }

  //parameter initialization
  def nextDouble = random.nextGaussian() * 0.1
  val init = Array(theta.createZeroSetting())
  init.head.vect.foreach(x => (0 until x.length).foreach(i => x.update(i, nextDouble)))
  init.head.mats.foreach(x => (0 until x.length).foreach(i => x.update(i, nextDouble)))


  val data: IndexedSeq[(Int, Int, Int)] = Array((0, 0, 0), (0, 0, 1))

  //loss definition
  val argmaxTerm = argmax(theta) { x => loss(data)(x).argmaxBy(Argmaxer.ascent(data.size * 1000, 0.1)(init)) }

  //training
  val t = argmaxTerm.eval()

  println(t)
}

