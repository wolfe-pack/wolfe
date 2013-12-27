package scalapplcodefest.example

import scalapplcodefest.value.Vectors
import scalapplcodefest.term.Max
import scalapplcodefest.{Index, TermDSL}
import scalapplcodefest.TermDSL._

/**
 * The smokes & cancer MLN.
 *
 * @author Sebastian Riedel
 */
object MarkovLogicExample {

  //this gives us syntactic sugar

  import TermDSL._

  //index for indexing feature vectors
  val key = new Index()

  //domain objects. Note that this set itself is a term (a constant evaluating to the given set, but it could be dynamic too).
  val persons = set('Anna, 'Bob)

  //Unary predicates
  val smokes = 'smokes of persons |-> bools
  val cancer = 'cancer of persons |-> bools

  //Binary predicate
  val friend = 'friend of c(persons, persons) |-> bools

  //Weight vector variable.
  val weights = 'weights of vectors

  //Smoking can lead to cancer
  val f1 = vectors.sum(for (p <- persons) yield unit(key('smokingIsBad),
    I(smokes(p) |=> cancer(p))))

  //friends make friends smoke / not smoke
  val f2 = vectors.sum(for ((p1,p2) <- c(persons, persons)) yield unit(key('peerPressure),
    I(friend(p1, p2) |=> (smokes(p1) <=> smokes(p2)))))

  //The MLN without assigned weights
  val mln = (f1 + f2) dot weights

  def main(args: Array[String]) {
    //an actual weight vector that can be plugged into the mln. todo: this should be created by terms
    val concreteWeights = key.createDenseVector(Seq('smokingIsBad) -> 1.0, Seq('peerPressure) -> 1.0)()

    //some observations
    val condition = state(friend.atom('Anna, 'Bob) -> true, smokes.atom('Anna) -> true)

    //the mln with weights and some ground atoms set to some observation
    val conditioned = mln | condition | weights -> concreteWeights

    //an inference result calculated through max product
    val argmax = Max.ByMessagePassing(lam(sig(smokes,cancer,friend),conditioned)).argmax.value()

    println(argmax)

  }

}
