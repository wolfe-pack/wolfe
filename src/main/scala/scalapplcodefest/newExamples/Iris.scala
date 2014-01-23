package scalapplcodefest.newExamples

import scalapplcodefest.sbt._
import scalapplcodefest._
import scala.io.Source
import scala.util.Random

/**
 * This example shows how to train and run a linear classifier on the IRIS dataset. This example intentionally uses
 * very little syntactic sugar or DSL code. Its purpose is to show the math behind a classifier.
 *
 * @author Sebastian Riedel
 */
@Compile
class Iris extends (() => Unit) {

  import Wolfe._
  import Iris._

  def apply() {
    //random generator for shuffling the data
    val random = new Random(0l)

    //the IRIS dataset
    val dataset = random.shuffle(Iris.loadIris())

    //train/test set split
    val (train, test) = dataset.splitAt(dataset.size / 2)

    //the set of all possible dates
    def sampleSpace = all(Data)(c(all(Observed)(c(doubles, doubles, doubles, doubles)), classes))

    //joint feature function on data (x,y)
    def features(data: Data) = oneHot('sl -> data.y, data.x.sepalLength) +
      oneHot('sw -> data.y, data.x.sepalWidth) +
      oneHot('pl -> data.y, data.x.petalLength) +
      oneHot('pw -> data.y, data.x.petalWidth)

    //the linear model
    def model(weights: Vector)(data: Data) = features(data) dot weights

    //a per-instance perceptron loss; it's gradient are the perceptron updates
    def loss(weights: Vector)(data: Data) = max(sampleSpace)(_.x == data.x)(model(weights)) - model(weights)(data)

    //the learned weights that minimize the perceptron loss
    val learned = argmin(vectors)(_ => true)(w => sum(train)(_ => true)({data => loss(w)(data)}))

    //the predictor using the learned weights
    def predict(data: Data) = argmax(sampleSpace)(_.x == data.x)(model(learned))

    //apply the predictor to each instance of the test set.
    val predicted = test.map(predict)

    println(learned)
    println(test.head)
    println(predicted.head)

    println(evaluate[Data](test,predicted)(_.y))

  }


}

@Analyze object Iris {

  type IrisClass = String
  val classes = Set("Iris-setosa", "Iris-versicolor", "Iris-virginica")

  case class Observed(sepalLength: Double, sepalWidth: Double, petalLength: Double, petalWidth: Double)

  case class Data(x: Observed, y: IrisClass)

  def loadIris() = {
    val stream = Util.getStreamFromClassPathOrFile("scalapplcodefest/datasets/iris/iris.data")
    val data = for (line <- Source.fromInputStream(stream).getLines().toBuffer if line.trim != "") yield {
      val Array(sl, sw, pl, pw, ic) = line.split(",")
      Data(Observed(sl.toDouble, sw.toDouble, pl.toDouble, pw.toDouble), ic)
    }
    stream.close()
    data
  }

  def evaluate[T](target: Seq[T], guess: Seq[T])(attribute: T => Any):Evaluation = {
    val evaluations = for ((t,g) <- target.view zip guess.view) yield evaluate(t, g)(attribute)
    val reduced = evaluations.foldLeft(Evaluation())(_ + _)
    reduced
  }

  def evaluate[T](target: T, guess: T)(attribute: T => Any):Evaluation =
    if (attribute(target) == attribute(guess)) Evaluation(tp = 1, tn = 1) else Evaluation(fp = 1, fn = 1)

  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/newExamples/Iris.scala",
      replacers = List(
        env => new ArgminByFactorieTrainerReplacer(env) with SimpleDifferentiator))

  }

}

//
