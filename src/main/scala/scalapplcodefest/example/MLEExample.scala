package scalapplcodefest.example

import math.log
import scalapplcodefest.example.Objective.{JointLoglikelihood, Adagrad, MaxProduct}


/**
 * @author Sebastian Riedel
 */
object MLEExample extends App {

  import WolfeEnv._

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

  import WolfeEnv._

  type Coin = Symbol

  //elements of the domain
  val coins = Set('H, 'T)

  //training data
  val data = Seq('H, 'T, 'T, 'T)

  @Objective.LinearModel
  def s(params: Vector)(coin: Coin) = ft(coin) dot params

  @Objective.JointLoglikelihood
  def ll(data: Seq[Coin])(w: Vector) = sum(data) {c => logZ(coins)(w) - s(w)(c)}

  //a subset of possible weights to make brute force search tractable
  val small = Set(0.0, 0.25, 0.75, 1.0).map(math.exp)

  //val w = argmin(vectors)(ll(data)) //this won't run without compilation
  val w = argmin(vectors(coins,small)) (ll(data))  //this should run w/o compilation

  println(w)

  //weight vector would not need to be normalized
  val probs = (w mapValues math.exp).norm

  println(probs)



}

object LanguageModel extends App {

  import WolfeEnv._

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

object MarginalsExample extends App {

  import WolfeEnv._

  case class Token(word: String, tag: String)

  type Sentence = Seq[Token]

  val tags = Set("NN", "DT")


}

object ConditioningExample extends App {

  import WolfeEnv._

  //idea: do the conditioning in the domain, not the function
  case class Data(x: Double, y: Double)

  def space = for (x <- Set(0.0, 1.0, 2.0); y <- Set(0.0, 1.0, 2.0)) yield Data(x, y)

  val result = argmax(space filter (_.x == 5))(data => -data.x * (data.y - 2))


}

