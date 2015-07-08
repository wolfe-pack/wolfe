package ml.wolfe.model

import java.io.File

import ml.wolfe.term.TermImplicits._
import ml.wolfe.term.{DefaultIndexer, AdaGradParameters, Argmaxer, LearningObjective}
import ml.wolfe.{SimpleIndex, Vect}

import scala.util.Random

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

  implicit def index: SimpleIndex

  implicit lazy val indexer = new DefaultIndexer(index)
  implicit val Labels = labels.toDom
  implicit val Xs = Vectors(maxFeats * labels.size)
  //todo: this is too large generally
  implicit val Instances = Pairs(Xs, Labels)
  implicit val random = new Random

  def model(t: Thetas.Term)(x: Xs.Term)(y: Labels.Term) = {
    t dot (x conjoin feature(y))
  }

  lazy val predict: Vect => Label = fun(Xs) { x => argmax(Labels)(model(Thetas.Const(thetaStar))(x)) }

  lazy val score: (Vect, Label) => Double = fun(Xs, Labels) { (x, y) => model(Thetas.Const(thetaStar))(x)(y) }


  /**
   * Takes an input X and returns its best label according to the classifier.
   * @param x the input feature vector
   * @return the label with maximal score.
   */
  def classify(x: X): Label = predict(x)

}

object LinearClassifier {

  import ml.wolfe.WolfePicklers._
  import scala.pickling.Defaults._
  import scala.pickling.binary._

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

      val index = new SimpleIndex
      lazy val thetaStar =
        learn(Thetas)(t => LearningObjective.perceptron(data.toConst)(Labels)(model(t))) using Argmaxer.adaGrad(params)
    }
  }

  /**
   * Datastructure to be pickled and unpickled for storing and loading of models.
   */
  case class Pickleable[L](labels: Seq[L], index: SimpleIndex, thetaStar: Vect)


  /**
   * Store the model to a file.
   * @param model model to store
   * @param file file to store to.
   * @tparam L type of labels.
   */
  def store[L](model: LinearClassifier[L], file: File): Unit = {
    writeToFile(Pickleable(model.labels, model.index, model.thetaStar).pickle, file)
  }

  /**
   * Load classifier from file.
   * @param file file to load from
   * @tparam L type of labels.
   * @return classifier stored in the given file.
   */
  def load[L](file: File): LinearClassifier[L] = {
    val m = loadBinary(file).unpickle[Pickleable[L]]
    new LinearClassifier[L] {
      def labels = m.labels

      def maxFeats = m.thetaStar.dim1

      lazy val index = m.index
      lazy val thetaStar = m.thetaStar
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