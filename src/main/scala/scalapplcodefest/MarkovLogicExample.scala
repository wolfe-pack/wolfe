package scalapplcodefest

import scalapplcodefest.value.Vectors

/**
 * The smokes & cancer MLN.
 *
 * @author Sebastian Riedel
 */
object MarkovLogicExample {

  //this gives us syntactic sugar

  import TermImplicits._

  //index for indexing feature vectors
  val key = new Index()

  //domain objects. Note that this set itself is a term (a constant evaluating to the given set, but it could be dynamic too).
  val persons = set('Anna, 'Bob)

  //Unary predicates
  val smokes = 'smokes of persons |-> bools
  val cancer = 'cancer of persons |-> bools

  //Binary predicate
  val friend = 'friend of (persons x persons) |-> bools

  //Weight vector variable.
  val weights = 'weights of Vectors

  //Smoking can lead to cancer
  val f1 = vsum(for (p <- persons) yield unit(key('smokingIsBad),
    I(smokes(p) |=> cancer(p))))

  //friends make friends smoke / not smoke
  val f2 = vsum(for (p1 <- persons; p2 <- persons) yield unit(key('peerPressure),
    I(friend(p1, p2) |=> (smokes(p1) <=> smokes(p2)))))

  //The MLN without assigned weights
  val mln = (f1 + f2) dot weights

  def main(args: Array[String]) {
    //an actual weight vector that can be plugged into the mln.
    val concreteWeights = key.createDenseVector(Seq('smokingIsBad) -> 1.0, Seq('peerPressure) -> 1.0)()

    //some observations
    val condition = state(friend.atom('Anna, 'Bob) -> true, smokes.atom('Anna) -> true)

    //the mln with weights and some ground atoms set to some observation
    val conditioned = mln | condition | weights -> concreteWeights

    //an inference result calculated through max product
    val argmax = Max.ByMessagePassing(conditioned).argmax.value()

    println(argmax)

  }

}
