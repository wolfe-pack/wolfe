package scalapplcodefest.macros

import scalapplcodefest.{MaxProduct, Wolfe}
import Wolfe._
import scala.util.Random
import scala.io.Source
import scalapplcodefest.util.{Util, Evaluator}
import cc.factorie.optimize.{Perceptron, OnlineTrainer}

/**
 * @author Sebastian Riedel
 */
object TestIris {

  import OptimizedWolfe._

  type IrisClass = String
  val classes = Seq("Iris-setosa", "Iris-versicolor", "Iris-virginica")

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

  def main(args: Array[String]) {
    //random generator for shuffling the data
    val random = new Random(0l)

    //the IRIS dataset
    val dataset = random.shuffle(loadIris())

    //train/test set split
    val (train, test) = dataset.splitAt(dataset.size / 2)

    //the set of all possible dates for a given observation
    def S(data:Data) = all2(Data)(c(Seq(data.x), classes))

    //joint feature function on data (x,y)
    def features(data: Data) =
      oneHot('sl -> data.y, data.x.sepalLength) +
      oneHot('sw -> data.y, data.x.sepalWidth) +
      oneHot('pl -> data.y, data.x.petalLength) +
      oneHot('pw -> data.y, data.x.petalWidth)

    //the linear model
    @MaxByInference(MaxProduct(_,1))
    def model(weights: Vector)(data: Data) = features(data) dot weights

    //the total training perceptron loss of the model given the weights
    @MinByDescent(new OnlineTrainer(_, new Perceptron, 4))
    def loss(weights: Vector) = sum(train)(_ => true)(i => max(S(i))(_ => true)(model(weights)) - model(weights)(i))

    //the learned weights that minimize the perceptron loss
    val learned = argmin(vectors)(_ => true)(loss)

    println("Learned:")
    println(learned)

    def predict(i: Data) = argmax(S(i))(_ => true)(model(learned))

    //apply the predictor to each instance of the test set.
    val predictedTest = test.map(predict)
    val predictedTrain = train.map(predict)


    println(predictedTest.head)

    println(Evaluator.evaluate(train,predictedTrain)(_.y))
    println(Evaluator.evaluate(test,predictedTest)(_.y))


  }
}
