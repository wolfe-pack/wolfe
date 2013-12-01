package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
object MarkovLogicExample {

  //this gives us syntactic sugar
  import TermImplicits._

  //index for indexing feature vectors
  val key = new Index()

  //domain objects. Note that this set itself is a term (a constant evaluating to the given set).
  val Persons = SetTerm('Anna, 'Bob)

  //Unary predicates
  val smokes = 'smokes of Persons |-> Bools
  val cancer = 'cancer of Persons |-> Bools

  //Binary predicate
  val friends = 'friend of (Persons x Persons) |-> Bools

  //Weight vector variable.
  val weights = 'weights of Vectors

  //Smoking can lead to cancer
  val f1 = vsum(for (p <- Persons) yield unit(key('smokingIsBad),
    I(smokes(p) |=> cancer(p))))

  //friends make friends smoke / not smoke
  val f2 = vsum(for (p1 <- Persons; p2 <- Persons) yield unit(key('peerPressure),
    I(friends(p1, p2) |=> (smokes(p1) <=> smokes(p2)))))

  //The MLN without assigned weights
  val mln = (f1 + f2) dot weights

  //an actual weight vector that can be plugged into the mln.
  val concreteWeights = key.createDenseVector(Seq('smokingIsBad) -> 1.0, Seq('peerPressure) -> 1.0)()

  def main(args: Array[String]) {
    //some observations
    val condition = State(Map(
      friends.atom('Anna,'Bob) -> true,
      smokes.atom('Anna) -> true))

    //the mln with weights and some ground atoms set to some observation
    val conditioned = mln | condition | weights -> concreteWeights

    //an inference result calculated through max product
    val inference = Inference.maxProductArgmax(2)(conditioned)

    println(inference.state())

  }

}
