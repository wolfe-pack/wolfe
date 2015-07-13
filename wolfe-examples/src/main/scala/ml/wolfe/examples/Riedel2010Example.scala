package ml.wolfe.examples

import ml.wolfe.SimpleFeatureIndex
import ml.wolfe.nlp.io.Riedel2010Reader
import ml.wolfe.term.TermImplicits._

object Riedel2010Example extends App {

  val d = Riedel2010Reader.read("/home/luke/Desktop/protobuff/train-Multiple.pb")

  import d.{Labels, labels, bags}
  type Bag = d.Bag

  implicit val Y = FullMaps(Labels, Bools)
  implicit val Z = Seqs(Labels, 0, bags.map(_.instances.length).max)

  implicit val Thetas = Vectors(1000)
  implicit val index = new SimpleFeatureIndex(Thetas)

  // Define model
   def model(theta:Thetas.Term)(y:Y.Term, z:Z.Term)(bag:Bag) = {
       sum(labels.indices) { r =>  phiJoin(y, labels(r), bag.label) } +
       sum(bag.instances.indices) { i => phiExtract(theta)(z(i), bag.instances(i)) }
   }

   def phiJoin(y:Y.Term, r:d.Labels.Term, z:d.Labels.Term) = {
     //todo: fix (y(r) === (z === r))
     logI((y(r) && (z === r)) || (!y(r) && !(z === r)))
   }

   def phiExtract(theta:Thetas.Term)(z_i: Labels.Term, x_i:Seq[String]) = {
     theta dot phi(z_i, x_i)
   }

   def phi(z_i: Labels.Term, x_i:Seq[String]): Thetas.Term = {
     //todo: implement VectorSum
//     sum(0 until m.feature.length) { i =>
//       feature('Foo, z_i, x_i(i))
//     }
     ???
   }


}
