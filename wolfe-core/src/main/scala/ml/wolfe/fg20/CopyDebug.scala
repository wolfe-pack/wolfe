package ml.wolfe.fg20

import cc.factorie.la.{DenseTensor1, SparseTensor1}
import cc.factorie.model.{WeightsMap, WeightsSet}
import cc.factorie.optimize.{BatchTrainer, AdaGrad}
import ml.wolfe.FactorieVector

import scala.util.Random

/**
 * @author Georgios Spithourakis
 */
object CopyDebug extends App{

  //A configuration that works:
  //type MyTensor=DenseTensor1 //optimisation with sparse weights does not work!!!
  val testCopy=false //set to true to test the .copy method of weightset

  //Test No1: WeightsSet.copy mutates the original weightsset...
  //type MyTensor=DenseTensor1
  //val testCopy=true

  //Test No2: SparseDensor1 gives nonsense results when used in optimisation (THIS DEMO DOES NOT WORK!)
  //Cannot replicate this with one dimensional vectors... Maybe with more dimensions, I'll look into it.
  type MyTensor=SparseTensor1
  //val testCopy=false

  val w=10.0
  val b=1.0

  def dataset(x:Seq[Double],noise:Double=0.0)={
    val r=new Random(100)
    val e=noise * r.nextGaussian()
    x.map(x=> (x,w*x+b+e))
  }

  val train=dataset(Seq(1.0,2.0,3.0))
  val test=dataset(Seq(1.5,2.5,3.5))

  println("---Train---")
  val regressor=LinearRegressor.learn(train)(x=>new DenseTensor1(Array(x)))

  println(s"w: ${regressor.weights.toArray.mkString(" ")}")
  println(s"b: ${regressor.bias}")

  println("---Test---")
  val predictions=test.map{case (x,y)=>(regressor(x),y)}
  println("prediction - real")
  for (pred<-predictions) println(pred.toString())

  def copyTestCode(weights: WeightsSet):Unit={
    def print(ws:WeightsSet):Unit={
      for ((k,i)<-ws.keys.zipWithIndex) println(s"weight no.$i: ${ws(k).toArray.mkString(" ")}")
    }
    println("-Test Copy-")
    println("Before: ")
    print(weights)
    val w=weights.copy //this is where I copy the weights. Copying changes the weights themselves!!! WeightsMap also implements the method with similar problems
    println("After:")
    print(weights)
  }

  class MyAdaGrad extends AdaGrad{
    override def processGradient(weights: WeightsSet, gradient: WeightsMap): Unit ={ //this is called every update
      if (testCopy) copyTestCode(weights)
      super.processGradient (weights, gradient)
    }
  }

  //From here below, I just define the regression models, potentials etc needed above

  class LinearRegressor[X](val weights: FactorieVector, val bias: Double, val feat: X => FactorieVector) extends (X => Double) {
    def apply(x: X): Double = (feat(x) dot weights) + bias
  }

  object LinearRegressor {

    def learn[X](data: Seq[(X, Double)])(feat: X => FactorieVector): LinearRegressor[X] = {

      val instances = data.toList.map({ case (x, y) => y -> feat(x)})
      val maxDim = instances.iterator.map(_._2.dim1).max
      val weightsVar = new VectVar(maxDim, "w")
      val biasVar = new ContVar("b")

      val loss = for ((y, f) <- instances) yield new L2NormLoss(weightsVar, biasVar, f, y)

      val initWeight= new MyTensor(maxDim)
      val init = Map(biasVar.asInstanceOf[Var[Any]]->0.0, weightsVar.asInstanceOf[Var[Any]] ->initWeight)

      val problem = Problem(loss)
      val optimizer = new GradientBasedOptimizer(problem)
      val result = optimizer.gradientBasedArgmax(new BatchTrainer(_, new MyAdaGrad, 100),State(init))
      new LinearRegressor(result.state(weightsVar), result.state(biasVar), feat)
    }
  }

  class L2NormLoss(val w: VectVar, val b:ContVar, val feats: FactorieVector, val y: Double,val scale:Double=1.0) extends L2NormLossLike{
    val contVars = Array(b)
    protected def bias(currentParameters:Setting) = currentParameters.cont(0)
    protected def updateGradient(gradient: Setting,guess:Double)={
      gradient.vect(0)=weightUpdate(gradient,guess)
      gradient.cont(0) =biasUpdate(gradient,guess)
    }
  }
  trait L2NormLossLike extends StatelessDifferentiable with StatelessScorer with Potential {

    val w: VectVar
    val feats: FactorieVector
    val y: Double
    val scale: Double

    val discVars = Potential.emptyDiscVars
    val vectVars = Array(w)

    //set these for models with bias
    //val contVars = Potential.emptyContVars //Array(b)
    protected def bias(currentParameters: Setting):Double
    protected def updateGradient(gradient: Setting,guess:Double):Unit

    final protected def weightUpdate(gradient: Setting,guess:Double)= feats * ( scale * 2.0 * (y - guess))
    final protected def biasUpdate(gradient: Setting,guess:Double)= scale * 2.0 * (y - guess)

    final def predict(currentParameters: Setting) = (feats dot currentParameters.vect(0)) + bias(currentParameters)
    final def score(guess:Double) = - (y - guess) * (y - guess) * scale

    final def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = {
      val guess = predict(currentParameters)
      updateGradient(gradient,guess)
      score(guess)
    }
    def score(currentParameters:Setting) = score(predict(currentParameters))
  }

}
