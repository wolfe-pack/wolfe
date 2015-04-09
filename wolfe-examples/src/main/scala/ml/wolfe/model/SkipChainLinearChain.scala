package ml.wolfe.model

import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe.{SimpleIndex, Vect}

/**
 * A simple linear chain labelling model.
 * @author riedel
 */
trait SkipChainLinearChain[Label] {
  import Argmaxer._
  import SkipChainLinearChain._
  
  def labels: Seq[Label]
  def maxFeats: Int
  def thetaStar: Vect
  def maxLength: Int

  implicit val maxProductParams = MaxProductParameters(10)

  implicit val Thetas = Vectors(maxFeats)
  implicit val index = new DefaultIndexer()
  implicit val Labels = labels.toDom
  implicit val Inputs = Input.Values(
    Seqs(Vectors(maxFeats),0,maxLength),
    Seqs(Vectors(maxFeats),0,maxLength),
    Seqs(Pairs(Ints,Ints),0,10))
  implicit val Outputs = Seqs(Labels,0,maxLength)
  implicit val Instances = Pairs(Inputs, Outputs)

  def crf(t: Thetas.Term)(x: Inputs.Term)(y: Outputs.Term) = {
    val local = sum(0 until x.unary.length)(i => t dot (x.unary(i) conjoin feature(y(i))))
    val pairwise =  sum(0 until x.unary.length - 1)(i => t dot (x.binary(i) conjoin feature(y(i) -> y(i+1))))
    (local + pairwise) argmaxBy maxProduct
  }

  def skip(x: Inputs.Term)(y: Outputs.Term) = {
    sum(x.matches) {p => 2.0 * I(y(p._1) === y(p._2))}
  }

  def skipChain(t: Thetas.Term)(x: Inputs.Term)(y: Outputs.Term) =
    crf(t)(x)(y) + skip(x)(y)

  val prediction = argmax(Outputs)(skipChain(thetaStar)(doc.words))

  lazy val predict =
    fun(Inputs) { x => argmax(Outputs)(crf(Thetas.Const(thetaStar))(x))}

  def classify(input:Input):Output[Label] = predict(input)
}

object SkipChainLinearChain {
  import LearningObjective._

  type Output[L] = IndexedSeq[L]
  @domain case class Input(unary:IndexedSeq[Vect],binary:IndexedSeq[Vect],
                           matches:IndexedSeq[(Int,Int)])

  def train[L](data:Seq[(Input,Output[L])],classLabels: Seq[L],
               params: AdaGradParameters,
               maxFeatures: Int = 1000, chainMaxLength:Int = 100):SkipChainLinearChain[L] = new SkipChainLinearChain[L] {
    lazy val thetaStar =
      learn(Thetas)(t => perceptron(data.toConst)(Outputs)(crf(t))) using Argmaxer.adaGrad(params)

    def labels = classLabels
    def maxFeats = maxFeatures
    def maxLength = chainMaxLength
  }

}



