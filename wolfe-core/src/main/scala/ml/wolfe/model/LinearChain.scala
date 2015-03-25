package ml.wolfe.model

import ml.wolfe.{SimpleIndex, Vect}
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term.{Argmaxer, MaxProductParameters, domain, AdaGradParameters}

/**
 * A simple linear chain labelling model.
 * @author riedel
 */
trait LinearChain[Label] {
  import LinearChain._
  import Argmaxer._
  
  def labels: Seq[Label]
  def maxFeats: Int
  def thetaStar: Vect
  def maxLength: Int
  def maxProductParams:MaxProductParameters

  implicit val Thetas = Vectors(maxFeats)
  implicit val index = new SimpleIndex
  implicit val Labels = labels.toDom
  implicit val Inputs = Input.Values(
    Seqs(Vectors(maxFeats * labels.size),0,maxLength),
    Seqs(Vectors(maxFeats * labels.size),0,maxLength))
  implicit val Outputs = Seqs(Labels,0,maxLength)
  implicit val Instances = Pairs(Inputs, Labels)


  def model(t: Thetas.Term)(x: Inputs.Term)(y: Outputs.Term) = {
    sum(0 until x.unary.length)(i => t dot (x.unary(i) conjoin feature(y(i)))) +
      sum(0 until x.unary.length - 1)(i => t dot (x.binary(i) conjoin feature(y(i) -> y(i+1))))

  }

  lazy val predict =
    fun(Inputs) { x => argmax(Outputs)(model(Thetas.Const(thetaStar))(x)) by maxProduct(maxProductParams) }

  def classify(input:Input):Output[Label] = predict(input)
}

object LinearChain {
  type Output[L] = IndexedSeq[L]
  @domain case class Input(unary:IndexedSeq[Vect],binary:IndexedSeq[Vect])

  def train[L](data:Seq[(Input,Output[L])],lassLabels: Seq[L],
               params: AdaGradParameters,
               maxFeatures: Int = 1000):LinearChain[L] = ???

}
