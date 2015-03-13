package ml.wolfe.term

import ml.wolfe.util.ProgressBar

import scala.math._

case class AdaGradParameters(iterations: Int,
                             learningRate: Double,
                             delta: Double = 0.0,
                             initParams: Settings = new Settings(0),
                             epochHook: (IndexedSeq[Any], Int) => String = null,
                             adaptiveVectors: Boolean = true,
                             delays: Map[Atom[Dom], Int] = Map.empty,
                             optimizeTerm: Boolean = false)

/**
 * @author riedel
 */
class AdaGradArgmaxer(val objRaw: DoubleTerm,
                       val wrt: Seq[Var[Dom]],
                       val observed: Settings,
                       val msgs: Msgs)(implicit params: AdaGradParameters) extends Argmaxer {

  import params._

  val preprocess = optimizeTerm match {
    case true => Transformer.clean _ andThen Transformer.groundSums andThen Transformer.flattenSums
    case false => identity[DoubleTerm] _
  }
  val obj = preprocess(objRaw)

  val obsVars = obj.vars.filterNot(wrt.contains)
  //get differentiator

  val initParameters = if (initParams.isEmpty) Settings.fromSeq(wrt.map(_.domain.createZeroSetting())) else initParams

  val obs2full = VariableMapping(obsVars, obj.vars)
  val param2full = VariableMapping(wrt, obj.vars)

  val scale = new Setting(numCont = 1)

  val gradient = Settings.fromSeq(wrt.map(_.domain.createZeroSetting()))
  val momentum = Settings.fromSeq(wrt.map(_.domain.createZeroSetting()))

  val termsPerEpoch = Traversal.distinctSampleCount(obj)

  val epochs = iterations / termsPerEpoch
  var objAccumulator = 0.0

  val result = Settings.fromSeq(wrt.map(_.domain.createSetting()))

  obs2full.linkTargetsToSource(observed, result)

  if (adaptiveVectors) gradient.foreach(_.setAdaptiveVectors(true))

  gradient foreach (_.recordChangedOffsets = true)
  result foreach (_.recordChangedOffsets = true)

  param2full.copyForwardDeep(initParameters, result)

  val diff = obj.differentiatorImpl(wrt)(result, scale, gradient)

  def argmax()(implicit execution: Execution) = {
    val bar = new ProgressBar(epochs, if (epochs < 100) 1 else epochs / 100)
    bar.start()

    scale.cont(0) = 1.0

    //now optimize
    for (iteration <- 0 until iterations) {
      val epoch = iteration / termsPerEpoch

      //reset all previous changes to the gradient
      gradient foreach (_.resetToZero())

      //add term gradient into result gradient
      diff.differentiate()(Execution(iteration, Execution.Diff))

      //now update the momentum, need to call atoms again because atoms may have changed if objective is dynamic
      addSquared(gradient, momentum)

      //remember changes for result from here on.
      result foreach (_.clearChangeRecord())

      //now add gradient into result parameters, using momentum to determine learning rate.
      gradientStep(epoch, gradient, momentum, result, learningRate)

      objAccumulator += diff.output.cont(0)

      if ((iteration + 1) % termsPerEpoch == 0) {
        if (epochHook != null) {
          val parameters = for ((v, s) <- wrt zip result) yield v.domain.toValue(s)
          val text = epochHook(parameters.toIndexedSeq, (iteration + 1) / termsPerEpoch)
          bar(s"Obj: $objAccumulator $text", lineBreak = true)
        } else {
          bar(s"Obj: $objAccumulator", lineBreak = true)
        }
        objAccumulator = 0.0
      }



    }
  }


  def addSquared(gradient: Settings, result: Settings): Unit = {
    for (i <- 0 until gradient.length) {
      for (offset <- gradient(i).cont.changed()) {
        val current = gradient(i).cont(offset)
        result(i).cont(offset) += current * current
      }
      for (offset <- gradient(i).vect.changed()) {
        val current = gradient(i).vect(offset)
        val targetVector = result(i).vect(offset)
        for (j <- current.activeDomain) {
          targetVector(j) += current(j) * current(j)
        }
      }
      for (offset <- gradient(i).mats.changed()) {
        val current = gradient(i).mats(offset)
        val targetMatrix = result(i).mats(offset)
        //todo: probably slow
        for (i1 <- 0 until current.dim1; i2 <- 0 until current.dim2)
          targetMatrix(i1, i2) += current(i1, i2) * current(i1, i2)
      }
    }
  }

  def gradientStep(epoch: Int, gradient: Settings, momentum: Settings, result: Settings, lambda: Double): Unit = {
    for (i <- 0 until gradient.length) {
      for (offset <- gradient(i).cont.changed()) {
        val g = gradient(i).cont(offset)
        val h = momentum(i).cont(offset)
        result(i).cont(offset) += lambda / (sqrt(h) + delta) * g
        result(i).cont.recordChange(offset)

      }
      for (offset <- gradient(i).vect.changed()) {
        val g = gradient(i).vect(offset)
        val h = momentum(i).vect(offset)
        for (j <- g.activeDomain) {
          result(i).vect(offset)(j) += lambda / (sqrt(h(j)) + delta) * g(j)
        }
        result(i).vect.recordChange(offset)
      }
      for (offset <- gradient(i).mats.changed()) {
        val g = gradient(i).mats(offset)
        val h = momentum(i).mats(offset)
        //todo: probably slow
        for (i1 <- 0 until g.dim1; i2 <- 0 until g.dim2)
          result(i).mats(offset)(i1, i2) += lambda / (sqrt(h(i1, i2)) + delta) * g(i1, i2)
        result(i).mats.recordChange(offset)

      }
    }
  }


}
