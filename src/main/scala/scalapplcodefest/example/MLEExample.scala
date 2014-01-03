package scalapplcodefest.example

import math.log
import scalapplcodefest._


/**
 * @author Sebastian Riedel
 */
object MLEExample extends App {

  import Wolfe._

  //training data
  val data = Seq('H, 'T, 'T, 'T)

  //elements of the domain
  val coins = Set('H, 'T)

  //log-likelihood objective
  @Objective.LogLikelihood
  def ll(data: Seq[Symbol])(prob: Symbol => Double) = sum(data) {x => log(prob(x))}

  //the ML estimate
  val p = argmax(simplex(coins, Set(0.0, 0.25, 0.75, 1.0))) {p => ll(data)(p)}

  println(p('T))

}

object MLEExampleWithLinearModel extends App {

  import Wolfe._

  type Coin = Symbol

  //elements of the domain
  val coins = Set('H, 'T)

  //training data
  val data = Seq('H, 'T, 'T, 'T)

  @Objective.Categorical
  def s(params: Vector)(coin: Coin) = ft(coin) dot params

  @Objective.JointLoglikelihood
  def ll(data: Seq[Coin])(w: Vector) = sum(data) {c => logZ(coins)(s(w)) - s(w)(c)}

  //a subset of possible weights to make brute force search tractable
  val myReals = Set(0.0, 0.25, 0.75, 1.0).map(math.exp)

  //val w = argmin(vectors)(ll(data)) //this won't run without compilation
  val w = argmin(vectors(coins, myReals)) {ll(data)} //this should run w/o compilation

  //this is how the compiled expression should look like
  val compiled = (data map (ft(_))).sum.mapValues(w => log(w / data.size))

  println(w)
  println(compiled)

  println(ll(data)(w))
  println(ll(data)(compiled))

  //weight vector would not need to be normalized
  val probs = (w mapValues math.exp).norm
  val probs2 = (compiled mapValues math.exp).norm


  println(probs)
  println(probs2)


}

object BayesianNetworkExample extends App {

  import Wolfe._

  case class Data(sprinkler: Boolean, wet: Boolean, rain: Boolean)

  val sampleSpace = for (s <- bools; w <- bools; r <- bools) yield Data(s, w, r)

  def stats(data: Data) = {
    import data._
    feat('r, rain) + feat('rs, rain, sprinkler) + feat('srw, sprinkler, rain, wet)
  }

  def bn(w:Vector)(data:Data) = stats(data) dot w

  def learn(data:Seq[Data]):Data => String = ??? //"feat('r, rain) + feat('rs, rain, sprinkler) + feat('srw, sprinkler, rain, wet)"


}

object LanguageModel extends App {

  import Wolfe._

  type Sentence = Seq[String]
  type BiGramModel = String => String => Double

  val data = Seq(Seq("the", "cat", "sat"), Seq("the", "brown", "fox"))
  val vocab = data.flatMap(identity).toSet

  def prob(s: Sentence)(p: BiGramModel) = Range(1, s.size).map(i => p(s(i - 1))(s(i))).product

  @Objective.LogLikelihood
  def ll(data: Seq[Sentence])(p: BiGramModel) = sum(data) {x => log(prob(x)(p))}

  val p = argmax(vocab -> simplex(vocab)) {ll(data)}

  println(p("cat")("the"))

  //println(p("cat" | "the")) this compiles but gives red code in IDEA

}


object ConditioningExample extends App {

  import Wolfe._

  //idea: do the conditioning in the domain, not the function
  case class Data(x: Double, y: Double)

  def space = for (x <- Set(0.0, 1.0, 2.0); y <- Set(0.0, 1.0, 2.0)) yield Data(x, y)

  val result = argmax(space filter (_.x == 5))(data => -data.x * (data.y - 2))


}

