package ml.wolfe.model

import ml.wolfe.term.TermImplicits._
import ml.wolfe.term.{AdaGradParameters, Argmaxer, LearningObjective}
import ml.wolfe.{SimpleIndex, Vect}

/**
 * Simple wrapper around linear models to simplify the use of classifiers.
 * @author riedel
 */
trait LinearClassifier[Label] {

  def labels: Seq[Label]
  def maxFeats: Int
  def thetaStar: Vect

  type Instance = (Vect, Label)
  type X = Vect
  val Thetas = Vectors(maxFeats)
  implicit val index = new SimpleIndex
  implicit val Labels = labels.toDom
  implicit val Xs = Vectors(maxFeats * labels.size)
  //todo: this is too large generally
  implicit val Instances = Pairs(Xs, Labels)

  def model(t: Thetas.Term)(x: Xs.Term)(y: Labels.Term) = {
    t dot (x conjoin feature(y))
  }

  lazy val predict = fun(Xs) { x => argmax(Labels)(model(Thetas.Const(thetaStar))(x)) }

  /**
   * Takes an input X and returns its best label according to the classifier.
   * @param x the input feature vector
   * @return the label with maximal score.
   */
  def classify(x: X): Label = predict(x)

}

object LinearClassifier {

  /**
   * Train a classifier
   * @param data pairs of feature vector observations and labels.
   * @param classLabels the sequence of class lables to choose from.
   * @param params adagrad training parameters.
   * @param maxFeatures max. number of features
   * @tparam L the type of labels.
   * @return a linear classifier.
   */
  def train[L](data: Seq[(Vect, L)],
               classLabels: Seq[L],
               params: AdaGradParameters,
               maxFeatures: Int = 1000): LinearClassifier[L] = {

    new LinearClassifier[L] {
      def labels = classLabels
      def maxFeats = maxFeatures
      lazy val thetaStar =
        learn(Thetas)(t => LearningObjective.perceptron(data.toConst)(Labels)(model(t))) using Argmaxer.adaGrad(params)
    }
  }

}

/**
 * Example usage of a classifier.
 */
object LinearClassifierExample {
  def main(args: Array[String]) {
    implicit val index = new SimpleIndex
    val data = Seq(feats("A") -> true, feats("B") -> false)
    val labels = Seq(false, true)
    val classifier = LinearClassifier.train(data, labels, AdaGradParameters(10, 0.1))
    println(classifier.classify(feats("A")))
    println(classifier.classify(feats("B")))

  }
}