package ml.wolfe.neural

import ml.wolfe.fg.Potential
import ml.wolfe.neural.io.MNIST

/**
 * Created by narad on 11/5/14.
 */
class BackPropagation() extends Potential  {

  override def mapF2N(): Unit = super.mapF2N()

}

object ExampleDBN extends App {

  case class X()
  case class Y()

  val train = MNIST.mlpTest //Seq(args(0))
  val test = Seq(args(1))

//  def model(w: Vector)(x: X)(y: Y) = (feats(x)(y) dot w)

}