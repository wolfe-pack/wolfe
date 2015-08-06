package ml.wolfe.examples

import ml.wolfe.SimpleFeatureIndex
import ml.wolfe.nlp.io.Riedel2010Reader
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._

object Riedel2010Example extends App {

  val x: Doubles.Term = ???
  x + x

  val d = Riedel2010Reader.read("/home/luke/Desktop/protobuff/train-Multiple.pb")

  val Labels = d.allLabels.toDom withOOV "[OOV]"
  val labels = d.allLabels.toConst(Labels)
  val Features = d.allFeatures.toDom
  val Instances = Seqs(Features, 0, d.maxFeatures)
  val Instancess = Seqs(Instances, 0, d.maxInstances)

  implicit val Y = FullMaps(Labels, Bools)
  implicit val Z = Seqs(Labels, 0, 1234) //todo: not this

  implicit val Thetas = Vectors(1000)
  implicit val index = new SimpleFeatureIndex(Thetas)

  case class Bag(label: Labels.Term, instances: Instancess.Term)

  val bags = for(bag <- d.bags) yield Bag(
    label = Labels.Const(bag.label),
    instances = Instancess.Const(bag.instances.map(_.features))
  )

  // Define model
   def model(theta:Thetas.Term)(y:Y.Term, z:Z.Term)(bag:Bag) = {
       sum(labels) { l =>
         phiJoin(y, l, bag.label)
       } + sum(0 until bag.instances.length) { i =>
         phiExtract(theta)(z(i), bag.instances(i))
       }
   }

   def phiJoin(y:Y.Term, r:Labels.Term, z:Labels.Term) = {
     //todo: fix (y(r) === (z === r))
     logI((y(r) && (z === r)) || (!y(r) && !(z === r)))
   }

   def phiExtract(theta:Thetas.Term)(z_i: Labels.Term, x_i:Instances.Term) = {
     theta dot phi(z_i, x_i)
   }

   def phi(z_i: Labels.Term, x_i:Instances.Term): Thetas.Term = cached(z_i, x_i) {
     //todo: implement VectorSum
//     sum(0 until x_i.length) { f =>
//       feature('Foo, z_i, x_i(f))
//     }
     Thetas.Const(Thetas.zero)
   }


}
