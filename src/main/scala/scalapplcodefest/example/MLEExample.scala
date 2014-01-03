package scalapplcodefest.example

import math.log


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
  val p = argmax(simplex(coins)) {ll(data)}

  println(p('T))

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

  @Domain.PMF
  def models = (vocab -> (vocab -> doubles)) filter (
    p => vocab.forall(context => sum(vocab) {p(context)(_)} == 1.0 && forall(vocab) {p(context)(_) >= 0.0}))

  val p = argmax(models) {ll(data)}

  println(p("cat")("the"))

  //println(p("cat" | "the")) this compiles but gives red code in IDEA

}

object MarginalsExample extends App {
  import WolfeEnv._

  case class Token(word:String,tag:String)
  type Sentence = Seq[Token]

  val tags = Set("NN","DT")


}

object ConditioningExample extends App {
  import WolfeEnv._

  //idea: do the conditioning in the domain, not the function
  case class Data(x:Double,y:Double)

  def space = for (x <- Set(0.0,1.0,2.0); y <- Set(0.0,1.0,2.0)) yield Data(x,y)

  val result = argmax (space filter (_.x == 5)) (data => - data.x * (data.y - 2))



}

