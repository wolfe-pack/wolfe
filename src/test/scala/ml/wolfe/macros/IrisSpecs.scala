package ml.wolfe.macros

import ml.wolfe.{MaxProduct, Wolfe, WolfeSpec}
import scala.util.Random
import ml.wolfe.macros.OptimizedWolfe.MaxByInference

/**
 * @author Sebastian Riedel
 */
class IrisSpecs extends WolfeSpec {

  "A Iris Model" should {
    "give reasonable performance on the IRIS dataset " in {
      import ml.wolfe.util.Iris._
      import Wolfe._

      //random generator for shuffling the data
      val random = new Random(0l)

      //the IRIS dataset
      val dataset = random.shuffle(loadIris())

      //train/test set split
      val (train, test) = dataset.splitAt(dataset.size / 2)

      //sample space of all possible Iris data values
      def space = Wolfe.all(IrisData)

      //define what the observed part of the data is
      def observed(d:IrisData) = d.copy(irisClass = hide[Label])

      //feature function on data
      def features(data: IrisData) =
        oneHot('sl -> data.irisClass, data.sepalLength) +
        oneHot('sw -> data.irisClass, data.sepalWidth) +
        oneHot('pl -> data.irisClass, data.petalLength) +
        oneHot('pw -> data.irisClass, data.petalWidth)

      //the linear model
      @MaxByInference(MaxProduct(_,1))
      def model(weights: Vector)(data: IrisData) = features(data) dot weights

      val structure = MetaStructure.structure(space)

      println(structure.nodes().size)

    }


  }

}
